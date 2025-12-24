import json
import math
import sys
import time


def parse_args(argv):
    cfg = {
        "tier": "",
        "obs_n": 20000,
        "threads": 4,
        "iterations_fast": 1000,
        "iterations_medium": 20,
        "iterations_slow": 10,
        "repeats": 1,
    }
    it = iter(argv)
    for a in it:
        if a == "--tier":
            cfg["tier"] = next(it, "")
        elif a == "--obs-n":
            cfg["obs_n"] = int(next(it, str(cfg["obs_n"])))
        elif a == "--threads":
            cfg["threads"] = int(next(it, str(cfg["threads"])))
        elif a == "--iterations-fast":
            cfg["iterations_fast"] = int(next(it, str(cfg["iterations_fast"])))
        elif a == "--iterations-medium":
            cfg["iterations_medium"] = int(next(it, str(cfg["iterations_medium"])))
        elif a == "--iterations-slow":
            cfg["iterations_slow"] = int(next(it, str(cfg["iterations_slow"])))
        elif a == "--repeats":
            cfg["repeats"] = int(next(it, str(cfg["repeats"])))
    cfg["threads"] = max(1, cfg["threads"])
    cfg["repeats"] = max(1, cfg["repeats"])
    return cfg


def mean(xs):
    return sum(xs) / max(1, len(xs))


def stddev_sample(xs):
    n = len(xs)
    if n < 2:
        return 0.0
    mu = mean(xs)
    var = sum((x - mu) ** 2 for x in xs) / (n - 1)
    return math.sqrt(var)


def bench(name, iterations, repeats, fn):
    times = []
    meta = {}
    repeats = max(1, repeats)
    for _ in range(repeats):
        fn()  # warm-up once per repeat
        for _ in range(iterations):
            t0 = time.perf_counter()
            meta = fn() or {}
            t1 = time.perf_counter()
            times.append((t1 - t0) * 1000.0)

    avg = mean(times)
    sd = stddev_sample(times)
    cv = (sd / avg) * 100.0 if abs(avg) > 1e-12 else 0.0
    return {
        "name": name,
        "iterations": iterations,
        "repeats": repeats,
        "avg_time_ms": sum(times) / len(times),
        "min_time_ms": min(times),
        "max_time_ms": max(times),
        "stddev_time_ms": sd,
        "cv_pct": cv,
        "rss_mb": None,
        "meta": {str(k): str(v) for k, v in meta.items()},
    }


def synthetic_lab_results(n):
    obs = []
    for i in range(n):
        code = "HGB" if (i % 10) == 0 else "WBC"
        obs.append(
            {
                "id": f"obs-{i}",
                "code": code,
                "value": float(i % 100) / 10.0,
                "unit": "g/dL",
            }
        )
    return obs


def validate_observation(o):
    return (
        isinstance(o.get("id"), str)
        and isinstance(o.get("code"), str)
        and isinstance(o.get("value"), (int, float))
        and isinstance(o.get("unit"), str)
    )


def main():
    cfg = parse_args(sys.argv[1:])
    results = []

    tier = cfg.get("tier", "")
    obs_n = int(cfg.get("obs_n", 20000))
    threads = int(cfg.get("threads", 4))
    repeats = int(cfg.get("repeats", 1))
    it_fast = int(cfg.get("iterations_fast", 1000))
    it_med = int(cfg.get("iterations_medium", 20))
    it_slow = int(cfg.get("iterations_slow", 10))

    def add_common_meta(m):
        m = dict(m or {})
        if tier:
            m["tier"] = tier
        m["obs_n"] = obs_n
        m["threads"] = threads
        return m

    def keyword_scan():
        text = "Patient SSN: 123-45-6789. social security number present."
        hits = 0
        for kw in ("ssn", "social security"):
            if kw in text.lower():
                hits += 1
        return add_common_meta({"hits": hits})

    def stats_mean():
        xs = [(i % 100) / 10.0 for i in range(5000)]
        m = sum(xs) / len(xs)
        return add_common_meta({"mean": f"{m:.6f}"})

    def dot_product():
        a = [0.4, 0.2, 0.9]
        w = [0.2, 0.5, 0.3]
        score = sum(x * y for x, y in zip(a, w)) + 0.1
        return add_common_meta({"score": f"{score:.6f}"})

    def observation_validate_query():
        obs = synthetic_lab_results(obs_n)
        for o in obs:
            if not validate_observation(o):
                raise RuntimeError("invalid")
        matched = [o for o in obs if o.get("code") == "HGB"]
        return add_common_meta({"observations": len(obs), "matched": len(matched)})

    def observation_ndjson_roundtrip():
        obs = synthetic_lab_results(obs_n)
        nd = "\n".join(json.dumps(o, separators=(",", ":")) for o in obs)
        back = [json.loads(line) for line in nd.splitlines() if line]
        return add_common_meta({"bytes": len(nd), "items": len(back)})

    def observation_validate_query_parallel():
        from concurrent.futures import ThreadPoolExecutor

        obs = synthetic_lab_results(obs_n)
        workers = max(1, min(threads, len(obs) if obs else 1))
        chunk = (len(obs) + workers - 1) // workers

        def validate_chunk(start):
            end = min(start + chunk, len(obs))
            for i in range(start, end):
                if not validate_observation(obs[i]):
                    raise RuntimeError("invalid")

        with ThreadPoolExecutor(max_workers=workers) as ex:
            futures = [ex.submit(validate_chunk, w * chunk) for w in range(workers)]
            for f in futures:
                f.result()

        matched = [o for o in obs if o.get("code") == "HGB"]
        return add_common_meta(
            {"observations": len(obs), "matched": len(matched), "workers": workers}
        )

    results.append(bench("python_keyword_scan", it_fast, repeats, keyword_scan))
    results.append(bench("python_mean", it_fast, repeats, stats_mean))
    results.append(bench("python_dot_product", it_fast, repeats, dot_product))

    results.append(bench("python_observation_validate_query", it_med, repeats, observation_validate_query))
    results.append(bench("python_observation_ndjson_roundtrip", it_slow, repeats, observation_ndjson_roundtrip))
    results.append(
        bench(
            "python_observation_validate_query_parallel",
            it_slow,
            repeats,
            observation_validate_query_parallel,
        )
    )

    out = {
        "name": "python_workloads",
        "iterations": 0,
        "repeats": 0,
        "avg_time_ms": 0.0,
        "min_time_ms": 0.0,
        "max_time_ms": 0.0,
        "stddev_time_ms": 0.0,
        "cv_pct": 0.0,
        "rss_mb": None,
        "meta": {"subresults": json.dumps(results)},
    }

    print(json.dumps(out))


if __name__ == "__main__":
    main()

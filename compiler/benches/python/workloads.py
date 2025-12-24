import json
import time


def bench(name, iterations, fn):
    fn()  # warm-up
    times = []
    meta = {}
    for _ in range(iterations):
        t0 = time.perf_counter()
        meta = fn() or {}
        t1 = time.perf_counter()
        times.append((t1 - t0) * 1000.0)
    return {
        "name": name,
        "iterations": iterations,
        "avg_time_ms": sum(times) / len(times),
        "min_time_ms": min(times),
        "max_time_ms": max(times),
        "rss_mb": None,
        "meta": {str(k): str(v) for k, v in meta.items()},
    }


def main():
    results = []

    def keyword_scan():
        text = "Patient SSN: 123-45-6789. social security number present."
        hits = 0
        for kw in ("ssn", "social security"):
            if kw in text.lower():
                hits += 1
        return {"hits": hits}

    def stats_mean():
        xs = [(i % 100) / 10.0 for i in range(5000)]
        m = sum(xs) / len(xs)
        return {"mean": f"{m:.6f}"}

    def dot_product():
        a = [0.4, 0.2, 0.9]
        w = [0.2, 0.5, 0.3]
        score = sum(x * y for x, y in zip(a, w)) + 0.1
        return {"score": f"{score:.6f}"}

    results.append(bench("python_keyword_scan", 200, keyword_scan))
    results.append(bench("python_mean", 500, stats_mean))
    results.append(bench("python_dot_product", 2000, dot_product))

    out = {
        "name": "python_workloads",
        "iterations": 0,
        "avg_time_ms": 0.0,
        "min_time_ms": 0.0,
        "max_time_ms": 0.0,
        "rss_mb": None,
        "meta": {"subresults": json.dumps(results)},
    }

    print(json.dumps(out))


if __name__ == "__main__":
    main()

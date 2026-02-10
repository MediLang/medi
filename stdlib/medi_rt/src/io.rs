use std::io;
use std::net::{TcpStream, ToSocketAddrs};
use std::time::Duration;

pub fn tcp_connect_timeout<A: ToSocketAddrs>(addr: A, timeout: Duration) -> io::Result<TcpStream> {
    let mut last_err: Option<io::Error> = None;
    for a in addr.to_socket_addrs()? {
        match TcpStream::connect_timeout(&a, timeout) {
            Ok(s) => return Ok(s),
            Err(e) => last_err = Some(e),
        }
    }
    Err(last_err.unwrap_or_else(|| io::Error::other("no addresses")))
}

pub fn set_low_latency(stream: &TcpStream) -> io::Result<()> {
    stream.set_nodelay(true)?;
    Ok(())
}

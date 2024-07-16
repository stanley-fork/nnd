use std::{
    future::Future,
    pin::Pin,
    task::{Context, Poll, Waker, RawWaker, RawWakerVTable},
};

struct ManualExecutor {
    future: Pin<Box<dyn Future<Output = ()>>>,
}

impl ManualExecutor {
    fn run(future: impl Future<Output = ()> + 'static) -> Self {
        Self {
            future: Box::pin(future),
        }
    }

    fn step(&mut self) -> bool {
        let waker = unsafe { Waker::from_raw(dummy_raw_waker()) };
        let mut cx = Context::from_waker(&waker);

        match self.future.as_mut().poll(&mut cx) {
            Poll::Ready(()) => false,
            Poll::Pending => true,
        }
    }
}

fn dummy_raw_waker() -> RawWaker {
    unsafe fn clone(_: *const ()) -> RawWaker {
        dummy_raw_waker()
    }
    unsafe fn wake(_: *const ()) {}
    unsafe fn wake_by_ref(_: *const ()) {}
    unsafe fn drop(_: *const ()) {}

    RawWaker::new(std::ptr::null(), &RawWakerVTable::new(clone, wake, wake_by_ref, drop))
}

struct Suspend {
    polled: bool,
}

impl Future for Suspend {
    type Output = ();

    fn poll(mut self: Pin<&mut Self>, _cx: &mut Context<'_>) -> Poll<()> {
        if self.polled {
            Poll::Ready(())
        } else {
            self.polled = true;
            Poll::Pending
        }
    }
}

fn suspend() -> Suspend {
    Suspend {polled: false}
}

async fn an_async_function() {
    println!("1");
    suspend().await;
    println!("3");
    suspend().await;
    println!("5");
}

fn main() {
    let mut exec = ManualExecutor::run(an_async_function());
    println!("0");
    assert!(exec.step());
    println!("2");
    assert!(exec.step());
    println!("4");
    assert!(!exec.step());
    println!("6 - done");
}


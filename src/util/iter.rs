pub trait ToSingle<T> {
    fn single(mut self) -> T;
}

impl<T> ToSingle<T> for Vec<T> {
    fn single(self) -> T {
        self.into_iter().next().unwrap()
    }
}

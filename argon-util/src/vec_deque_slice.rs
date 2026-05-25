use std::collections::VecDeque;

#[derive(Debug, PartialEq, Eq)]
pub struct VecDequeSlice<'a, T> {
    first: &'a [T],
    second: &'a [T],
}

impl<'a, T> VecDequeSlice<'a, T> {
    pub fn new(coll: &'a VecDeque<T>) -> Self {
        let (first, second) = coll.as_slices();
        Self { first, second }
    }

    pub fn split_first(&self) -> Option<(&'a T, Self)> {
        if let Some((item, first)) = self.first.split_first() {
            Some((
                item,
                Self {
                    first,
                    second: self.second,
                },
            ))
        } else {
            self.second.split_first().map(|(item, second)| {
                (
                    item,
                    Self {
                        first: self.first,
                        second,
                    },
                )
            })
        }
    }

    pub fn len(&self) -> usize {
        self.first.len() + self.second.len()
    }

    pub fn is_empty(&self) -> bool {
        self.first.is_empty() && self.second.is_empty()
    }
}



impl <'a, T> Clone for VecDequeSlice<'a, T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl <'a, T> Copy for VecDequeSlice<'a, T> {}

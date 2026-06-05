use alloc::{
    borrow::{Cow, ToOwned},
    collections::VecDeque,
    vec::Vec,
};

const INLINE_PARTS: usize = 3;

#[derive(Debug, Clone, PartialEq, Eq)]
enum MultiSlicePart<'a, T> {
    Slice(&'a [T]),
    Vec(Vec<T>),
}

impl<'a, T> MultiSlicePart<'a, T> {
    pub fn as_slice(&self) -> &[T] {
        match self {
            Self::Slice(slice) => slice,
            Self::Vec(vec) => vec.as_slice(),
        }
    }

    fn first(&self) -> Option<&T> {
        self.as_slice().first()
    }

    fn len(&self) -> usize {
        self.as_slice().len()
    }

    fn is_empty(&self) -> bool {
        self.as_slice().is_empty()
    }
}

impl<'a, T: ToOwned<Owned = T>> MultiSlicePart<'a, T> {
    fn pop(&mut self) -> Option<Cow<'a, T>> {
        match self {
            Self::Slice(slice) => {
                let (item, rest) = slice.split_last()?;
                *slice = rest;
                Some(Cow::Borrowed(item))
            }
            Self::Vec(vec) => vec.pop().map(Cow::Owned),
        }
    }

    fn pop_front(&mut self) -> Option<Cow<'a, T>> {
        match self {
            Self::Slice(slice) => {
                let (item, rest) = slice.split_first()?;
                *slice = rest;
                Some(Cow::Borrowed(item))
            }
            Self::Vec(vec) => {
                if vec.is_empty() {
                    None
                } else {
                    Some(Cow::Owned(vec.remove(0)))
                }
            }
        }
    }
}

impl<'a, T> AsRef<[T]> for MultiSlicePart<'a, T> {
    fn as_ref(&self) -> &[T] {
        self.as_slice()
    }
}

impl<'a, T> From<&'a [T]> for MultiSlicePart<'a, T> {
    fn from(value: &'a [T]) -> Self {
        Self::Slice(value)
    }
}

impl<'a, T> From<&'a Vec<T>> for MultiSlicePart<'a, T> {
    fn from(value: &'a Vec<T>) -> Self {
        Self::Slice(value.as_slice())
    }
}

impl<'a, T> From<Vec<T>> for MultiSlicePart<'a, T> {
    fn from(value: Vec<T>) -> Self {
        Self::Vec(value)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MultiSlice<'a, T> {
    parts: MultiSliceParts<'a, T>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum MultiSliceParts<'a, T> {
    Inline {
        len: usize,
        parts: [Option<MultiSlicePart<'a, T>>; INLINE_PARTS],
    },
    Heap(VecDeque<MultiSlicePart<'a, T>>),
}

impl<'a, T> MultiSliceParts<'a, T> {
    fn new() -> Self {
        Self::Inline {
            len: 0,
            parts: [None, None, None],
        }
    }

    fn with_capacity(capacity: usize) -> Self {
        if capacity <= INLINE_PARTS {
            Self::new()
        } else {
            Self::Heap(VecDeque::with_capacity(capacity))
        }
    }

    fn push(&mut self, part: MultiSlicePart<'a, T>) {
        match self {
            Self::Inline { len, parts } if *len < INLINE_PARTS => {
                parts[*len] = Some(part);
                *len += 1;
            }
            Self::Inline { len, parts } => {
                let mut heap_parts = VecDeque::with_capacity(*len + 1);
                for part in parts.iter_mut().take(*len) {
                    heap_parts.push_back(part.take().expect("inline part must be initialized"));
                }
                heap_parts.push_back(part);
                *self = Self::Heap(heap_parts);
            }
            Self::Heap(parts) => parts.push_back(part),
        }
    }

    fn push_front(&mut self, part: MultiSlicePart<'a, T>) {
        match self {
            Self::Inline { len, parts } if *len < INLINE_PARTS => {
                for index in (0..*len).rev() {
                    parts[index + 1] = parts[index].take();
                }
                parts[0] = Some(part);
                *len += 1;
            }
            Self::Inline { len, parts } => {
                let mut heap_parts = VecDeque::with_capacity(*len + 1);
                heap_parts.push_back(part);
                for part in parts.iter_mut().take(*len) {
                    heap_parts.push_back(part.take().expect("inline part must be initialized"));
                }
                *self = Self::Heap(heap_parts);
            }
            Self::Heap(parts) => parts.push_front(part),
        }
    }

    fn len_parts(&self) -> usize {
        match self {
            Self::Inline { len, .. } => *len,
            Self::Heap(parts) => parts.len(),
        }
    }

    fn part(&self, index: usize) -> Option<&MultiSlicePart<'a, T>> {
        match self {
            Self::Inline { len, parts } => {
                if index < *len {
                    parts[index].as_ref()
                } else {
                    None
                }
            }
            Self::Heap(parts) => parts.get(index),
        }
    }

    fn first(&self) -> Option<&T> {
        (0..self.len_parts()).find_map(|index| {
            self.part(index)
                .and_then(|part| (!part.is_empty()).then(|| part.first()).flatten())
        })
    }

    fn into_iter(self) -> MultiSlicePartsIntoIter<'a, T> {
        match self {
            Self::Inline { len, parts } => MultiSlicePartsIntoIter::Inline {
                len,
                index: 0,
                parts,
            },
            Self::Heap(parts) => MultiSlicePartsIntoIter::Heap(parts.into_iter()),
        }
    }
}

impl<'a, T: ToOwned<Owned = T>> MultiSliceParts<'a, T> {
    fn pop(&mut self) -> Option<Cow<'a, T>> {
        loop {
            match self {
                Self::Inline { len, .. } if *len == 0 => return None,
                Self::Inline { len, parts } => {
                    let index = *len - 1;
                    let part = parts[index]
                        .as_mut()
                        .expect("inline part must be initialized");
                    let item = part.pop();

                    if part.is_empty() {
                        parts[index] = None;
                        *len -= 1;
                    }

                    if item.is_some() {
                        return item;
                    }
                }
                Self::Heap(parts) => {
                    let part = parts.back_mut()?;
                    let item = part.pop();

                    if part.is_empty() {
                        parts.pop_back();
                    }

                    if item.is_some() {
                        return item;
                    }
                }
            }
        }
    }

    fn pop_front(&mut self) -> Option<Cow<'a, T>> {
        loop {
            match self {
                Self::Inline { len, .. } if *len == 0 => return None,
                Self::Inline { len, parts } => {
                    let part = parts[0].as_mut().expect("inline part must be initialized");
                    let item = part.pop_front();

                    if part.is_empty() {
                        for index in 1..*len {
                            parts[index - 1] = parts[index].take();
                        }
                        *len -= 1;
                    }

                    if item.is_some() {
                        return item;
                    }
                }
                Self::Heap(parts) => {
                    let part = parts.front_mut()?;
                    let item = part.pop_front();

                    if part.is_empty() {
                        parts.pop_front();
                    }

                    if item.is_some() {
                        return item;
                    }
                }
            }
        }
    }
}

#[derive(Debug, Clone)]
enum MultiSlicePartsIntoIter<'a, T> {
    Inline {
        len: usize,
        index: usize,
        parts: [Option<MultiSlicePart<'a, T>>; INLINE_PARTS],
    },
    Heap(alloc::collections::vec_deque::IntoIter<MultiSlicePart<'a, T>>),
}

impl<'a, T> Iterator for MultiSlicePartsIntoIter<'a, T> {
    type Item = MultiSlicePart<'a, T>;

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            Self::Inline { len, index, parts } => {
                if *index >= *len {
                    return None;
                }

                let part = parts[*index].take();
                *index += 1;
                part
            }
            Self::Heap(parts) => parts.next(),
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let len = match self {
            Self::Inline { len, index, .. } => len.saturating_sub(*index),
            Self::Heap(parts) => parts.len(),
        };
        (len, Some(len))
    }
}

impl<'a, T> ExactSizeIterator for MultiSlicePartsIntoIter<'a, T> {}

#[derive(Debug)]
struct MultiSlicePartsIter<'m, 'a, T> {
    parts: &'m MultiSliceParts<'a, T>,
    index: usize,
}

impl<'m, 'a, T> Clone for MultiSlicePartsIter<'m, 'a, T> {
    fn clone(&self) -> Self {
        Self {
            parts: self.parts,
            index: self.index,
        }
    }
}

impl<'m, 'a, T> Iterator for MultiSlicePartsIter<'m, 'a, T> {
    type Item = &'m MultiSlicePart<'a, T>;

    fn next(&mut self) -> Option<Self::Item> {
        let part = self.parts.part(self.index)?;
        self.index += 1;
        Some(part)
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let len = self.parts.len_parts().saturating_sub(self.index);
        (len, Some(len))
    }
}

impl<'m, 'a, T> ExactSizeIterator for MultiSlicePartsIter<'m, 'a, T> {}

impl<'a, T> MultiSlice<'a, T> {
    pub fn new() -> Self {
        Self {
            parts: MultiSliceParts::new(),
        }
    }

    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            parts: MultiSliceParts::with_capacity(capacity),
        }
    }

    pub fn push_slice(&mut self, slice: &'a [T]) {
        self.parts.push(slice.into());
    }

    pub fn push_vec(&mut self, vec: Vec<T>) {
        self.parts.push(vec.into());
    }

    pub fn push_vec_deque_slices(&mut self, vec_deque: &'a VecDeque<T>) {
        let (first, second) = vec_deque.as_slices();
        self.push_non_empty_slice(first);
        self.push_non_empty_slice(second);
    }

    pub fn push_front_slice(&mut self, slice: &'a [T]) {
        self.parts.push_front(slice.into());
    }

    pub fn push_front_vec(&mut self, vec: Vec<T>) {
        self.parts.push_front(vec.into());
    }

    pub fn push_front_vec_deque_slices(&mut self, vec_deque: &'a VecDeque<T>) {
        let (first, second) = vec_deque.as_slices();
        self.push_front_non_empty_slice(second);
        self.push_front_non_empty_slice(first);
    }

    fn push_non_empty_slice(&mut self, slice: &'a [T]) {
        if !slice.is_empty() {
            self.push_slice(slice);
        }
    }

    fn push_front_non_empty_slice(&mut self, slice: &'a [T]) {
        if !slice.is_empty() {
            self.push_front_slice(slice);
        }
    }

    fn parts_iter(&self) -> MultiSlicePartsIter<'_, 'a, T> {
        MultiSlicePartsIter {
            parts: &self.parts,
            index: 0,
        }
    }

    pub fn len(&self) -> usize {
        self.parts_iter().map(MultiSlicePart::len).sum()
    }

    pub fn is_empty(&self) -> bool {
        self.parts_iter().all(MultiSlicePart::is_empty)
    }

    pub fn first(&self) -> Option<&T> {
        self.parts.first()
    }

    pub fn iter(&self) -> MultiSliceIter<'_, 'a, T> {
        MultiSliceIter {
            parts: self.parts_iter(),
            current: [].iter(),
        }
    }
}

impl<'a, T: Clone> MultiSlice<'a, T> {
    pub fn pop(&mut self) -> Option<Cow<'a, T>> {
        self.parts.pop()
    }

    pub fn pop_front(&mut self) -> Option<Cow<'a, T>> {
        self.parts.pop_front()
    }
}

impl<'a, T> From<Vec<T>> for MultiSlice<'a, T> {
    fn from(vec: Vec<T>) -> Self {
        let mut ms = Self::new();
        ms.push_vec(vec);
        ms
    }
}

impl<'a, T> From<&'a [T]> for MultiSlice<'a, T> {
    fn from(slice: &'a [T]) -> Self {
        let mut ms = Self::new();
        ms.push_slice(slice);
        ms
    }
}

impl<'a, T> From<&'a VecDeque<T>> for MultiSlice<'a, T> {
    fn from(vec_deque: &'a VecDeque<T>) -> Self {
        let mut ms = Self::new();
        ms.push_vec_deque_slices(vec_deque);
        ms
    }
}

impl<'a, T> Default for MultiSlice<'a, T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<'m, 'a, T> IntoIterator for &'m MultiSlice<'a, T> {
    type Item = &'m T;
    type IntoIter = MultiSliceIter<'m, 'a, T>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

impl<'a, T: Clone> IntoIterator for MultiSlice<'a, T> {
    type Item = T;
    type IntoIter = MultiSliceIntoIter<'a, T>;

    fn into_iter(self) -> Self::IntoIter {
        MultiSliceIntoIter {
            parts: self.parts.into_iter(),
            current: None,
        }
    }
}

#[derive(Debug, Clone)]
pub struct MultiSliceIter<'m, 'a, T> {
    parts: MultiSlicePartsIter<'m, 'a, T>,
    current: core::slice::Iter<'m, T>,
}

impl<'m, 'a, T> Iterator for MultiSliceIter<'m, 'a, T> {
    type Item = &'m T;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if let Some(item) = self.current.next() {
                return Some(item);
            }

            self.current = self.parts.next()?.as_slice().iter();
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let current_len = self.current.len();
        let remaining_parts_len = self
            .parts
            .clone()
            .map(|part| part.as_slice().len())
            .sum::<usize>();
        let len = current_len + remaining_parts_len;
        (len, Some(len))
    }
}

impl<'m, 'a, T> ExactSizeIterator for MultiSliceIter<'m, 'a, T> {}

#[derive(Debug, Clone)]
pub struct MultiSliceIntoIter<'a, T> {
    parts: MultiSlicePartsIntoIter<'a, T>,
    current: Option<MultiSlicePartIntoIter<'a, T>>,
}

#[derive(Debug, Clone)]
enum MultiSlicePartIntoIter<'a, T> {
    Slice(core::iter::Cloned<core::slice::Iter<'a, T>>),
    Vec(alloc::vec::IntoIter<T>),
}

impl<'a, T: Clone> MultiSlicePartIntoIter<'a, T> {
    fn new(part: MultiSlicePart<'a, T>) -> Self {
        match part {
            MultiSlicePart::Slice(slice) => Self::Slice(slice.iter().cloned()),
            MultiSlicePart::Vec(vec) => Self::Vec(vec.into_iter()),
        }
    }

    fn len(&self) -> usize {
        match self {
            Self::Slice(iter) => iter.len(),
            Self::Vec(iter) => iter.len(),
        }
    }
}

impl<'a, T: Clone> Iterator for MultiSlicePartIntoIter<'a, T> {
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            Self::Slice(iter) => iter.next(),
            Self::Vec(iter) => iter.next(),
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let len = self.len();
        (len, Some(len))
    }
}

impl<'a, T: Clone> ExactSizeIterator for MultiSlicePartIntoIter<'a, T> {}

impl<'a, T: Clone> Iterator for MultiSliceIntoIter<'a, T> {
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if let Some(current) = &mut self.current {
                if let Some(item) = current.next() {
                    return Some(item);
                }
            }

            self.current = Some(MultiSlicePartIntoIter::new(self.parts.next()?));
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let current_len = self.current.as_ref().map_or(0, MultiSlicePartIntoIter::len);
        let remaining_parts_len = self
            .parts
            .clone()
            .map(|part| part.as_slice().len())
            .sum::<usize>();
        let len = current_len + remaining_parts_len;
        (len, Some(len))
    }
}

impl<'a, T: Clone> ExactSizeIterator for MultiSliceIntoIter<'a, T> {}

#[cfg(test)]
mod tests {
    use super::*;
    use alloc::collections::VecDeque;
    use alloc::vec;

    #[test]
    fn iterates_over_borrowed_and_owned_parts_by_ref() {
        let borrowed = [1, 2];
        let mut multi_slice = MultiSlice::new();
        multi_slice.push_slice(&borrowed);
        multi_slice.push_vec(vec![3, 4]);

        let values = multi_slice.iter().copied().collect::<Vec<_>>();

        assert_eq!(values, vec![1, 2, 3, 4]);
    }

    #[test]
    fn iterates_over_borrowed_and_owned_parts_by_value() {
        let borrowed = [1, 2];
        let mut multi_slice = MultiSlice::new();
        multi_slice.push_slice(&borrowed);
        multi_slice.push_vec(vec![3, 4]);

        let values = multi_slice.into_iter().collect::<Vec<_>>();

        assert_eq!(values, vec![1, 2, 3, 4]);
    }

    #[test]
    fn reports_len_and_empty_state() {
        let borrowed = [1, 2];
        let mut multi_slice = MultiSlice::new();

        assert!(multi_slice.is_empty());
        assert_eq!(multi_slice.len(), 0);

        multi_slice.push_slice(&[]);
        multi_slice.push_slice(&borrowed);
        multi_slice.push_vec(vec![3]);

        assert!(!multi_slice.is_empty());
        assert_eq!(multi_slice.len(), 3);
    }

    #[test]
    fn iterator_size_hint_tracks_remaining_items() {
        let borrowed = [1, 2];
        let mut multi_slice = MultiSlice::new();
        multi_slice.push_slice(&borrowed);
        multi_slice.push_vec(vec![3]);

        let mut iter = multi_slice.iter();

        assert_eq!(iter.size_hint(), (3, Some(3)));
        assert_eq!(iter.next(), Some(&1));
        assert_eq!(iter.size_hint(), (2, Some(2)));
    }

    #[test]
    fn keeps_three_or_fewer_parts_inline() {
        let borrowed = [1, 2];
        let mut multi_slice = MultiSlice::new();
        multi_slice.push_slice(&borrowed[..1]);
        multi_slice.push_slice(&borrowed[1..]);
        multi_slice.push_vec(vec![3]);

        assert!(matches!(
            multi_slice.parts,
            MultiSliceParts::Inline { len: 3, .. }
        ));
        assert_eq!(multi_slice.parts.len_parts(), 3);
    }

    #[test]
    fn moves_to_heap_after_three_parts() {
        let borrowed = [1, 2, 3, 4];
        let mut multi_slice = MultiSlice::new();
        multi_slice.push_slice(&borrowed[0..1]);
        multi_slice.push_slice(&borrowed[1..2]);
        multi_slice.push_slice(&borrowed[2..3]);
        multi_slice.push_slice(&borrowed[3..4]);

        assert!(matches!(multi_slice.parts, MultiSliceParts::Heap(_)));
        assert_eq!(
            multi_slice.iter().copied().collect::<Vec<_>>(),
            vec![1, 2, 3, 4]
        );
    }

    #[test]
    fn pushes_parts_to_front_inline() {
        let borrowed = [1, 3];
        let mut multi_slice = MultiSlice::new();
        multi_slice.push_slice(&borrowed[1..]);
        multi_slice.push_front_vec(vec![2]);
        multi_slice.push_front_slice(&borrowed[..1]);

        assert!(matches!(
            multi_slice.parts,
            MultiSliceParts::Inline { len: 3, .. }
        ));
        assert_eq!(
            multi_slice.iter().copied().collect::<Vec<_>>(),
            vec![1, 2, 3]
        );
    }

    #[test]
    fn pushing_to_front_moves_to_heap_after_three_parts() {
        let borrowed = [1, 2, 3, 4];
        let mut multi_slice = MultiSlice::new();
        multi_slice.push_slice(&borrowed[1..2]);
        multi_slice.push_slice(&borrowed[2..3]);
        multi_slice.push_slice(&borrowed[3..4]);
        multi_slice.push_front_slice(&borrowed[0..1]);

        assert!(matches!(multi_slice.parts, MultiSliceParts::Heap(_)));
        assert_eq!(
            multi_slice.iter().copied().collect::<Vec<_>>(),
            vec![1, 2, 3, 4]
        );
    }

    #[test]
    fn pushes_vec_deque_slices_to_back() {
        let prefix = [0];
        let vec_deque = split_vec_deque();
        let mut multi_slice = MultiSlice::new();
        multi_slice.push_slice(&prefix);
        multi_slice.push_vec_deque_slices(&vec_deque);

        assert_eq!(
            multi_slice.iter().copied().collect::<Vec<_>>(),
            vec![0, 1, 2, 3, 4]
        );
        assert_eq!(multi_slice.len(), 5);
    }

    #[test]
    fn pushes_vec_deque_slices_to_front() {
        let suffix = [5];
        let vec_deque = split_vec_deque();
        let mut multi_slice = MultiSlice::new();
        multi_slice.push_slice(&suffix);
        multi_slice.push_front_vec_deque_slices(&vec_deque);

        assert_eq!(
            multi_slice.iter().copied().collect::<Vec<_>>(),
            vec![1, 2, 3, 4, 5]
        );
    }

    #[test]
    fn gets_first_item() {
        let borrowed = [1, 2];
        let mut multi_slice = MultiSlice::new();
        multi_slice.push_slice(&[]);
        multi_slice.push_slice(&borrowed);
        multi_slice.push_vec(vec![3]);

        assert_eq!(multi_slice.first(), Some(&1));
    }

    #[test]
    fn pops_items_from_back() {
        let borrowed = [1, 2];
        let mut multi_slice = MultiSlice::new();
        multi_slice.push_slice(&borrowed);
        multi_slice.push_vec(vec![3, 4]);
        multi_slice.push_slice(&[]);

        assert_eq!(multi_slice.pop(), Some(Cow::Owned(4)));
        assert_eq!(multi_slice.pop(), Some(Cow::Owned(3)));
        assert_eq!(multi_slice.pop(), Some(Cow::Borrowed(&2)));
        assert_eq!(multi_slice.iter().copied().collect::<Vec<_>>(), vec![1]);
    }

    #[test]
    fn pops_items_from_front() {
        let borrowed = [1, 2];
        let mut multi_slice = MultiSlice::new();
        multi_slice.push_slice(&[]);
        multi_slice.push_slice(&borrowed);
        multi_slice.push_vec(vec![3, 4]);

        assert_eq!(multi_slice.pop_front(), Some(Cow::Borrowed(&1)));
        assert_eq!(multi_slice.pop_front(), Some(Cow::Borrowed(&2)));
        assert_eq!(multi_slice.pop_front(), Some(Cow::Owned(3)));
        assert_eq!(multi_slice.iter().copied().collect::<Vec<_>>(), vec![4]);
    }

    #[test]
    fn popping_until_empty_clears_all_parts() {
        let borrowed = [1];
        let mut multi_slice = MultiSlice::new();
        multi_slice.push_slice(&borrowed);
        multi_slice.push_vec(vec![2]);

        assert_eq!(multi_slice.pop_front(), Some(Cow::Borrowed(&1)));
        assert_eq!(multi_slice.pop(), Some(Cow::Owned(2)));
        assert_eq!(multi_slice.first(), None);
        assert_eq!(multi_slice.pop(), None);
        assert_eq!(multi_slice.pop_front(), None);
        assert!(multi_slice.is_empty());
        assert_eq!(multi_slice.parts.len_parts(), 0);
    }

    fn split_vec_deque() -> VecDeque<i32> {
        let mut vec_deque = VecDeque::with_capacity(4);
        vec_deque.push_back(0);
        vec_deque.push_back(1);
        vec_deque.push_back(2);
        vec_deque.push_back(3);
        assert_eq!(vec_deque.pop_front(), Some(0));
        vec_deque.push_back(4);

        let (first, second) = vec_deque.as_slices();
        assert!(!first.is_empty());
        assert!(!second.is_empty());

        vec_deque
    }
}

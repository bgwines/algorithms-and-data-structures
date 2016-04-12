
import pdb

class SegTree:
    '''
    (ctor)

    elems :: [a]
    f :: a -> a -> a
    '''
    def __init__(self, elems, f):
        size = 1
        while size <= len(elems):
            size *= 2
        size = (2 * size)

        self.tree = [None for _ in xrange(0, size)]
        for (i, e) in enumerate(elems):
            self.tree[self._get_leaf_offset() + i] = e

        self._f = f

        for node in xrange(self._get_leaf_offset() - 1, 0, -1):
            self.tree[node] = self._f(\
                self.tree[self._get_l(node)],\
                self.tree[self._get_r(node)]\
            )

        self._range_cache = dict()

    '''
    Gets the left child of `node` (where nodes are represented by indices into
    our array)

    _get_l :: int -> int
    '''
    def _get_l(self, node):
        return node * 2

    '''
    Gets the right child of `node` (where nodes are represented by indices into
    our array)

    _get_r :: int -> int
    '''
    def _get_r(self, node):
        return (node * 2) + 1 # array in order of BFS traversal of the tree

    '''
    We use an array to back the tree (in order of BFS traversal), so
    leaves are offset by some index.

    _get_leaf_offset :: int
    '''
    def _get_leaf_offset(self):
        return len(self.tree) / 2 # array in order of BFS traversal of the tree

    '''
    We use an array to back the tree (in order of BFS traversal). Some
    indices correspond to nodes, but some don't, depending on the index
    and size of the tree.

    _in_range :: int -> bool
    '''
    def _in_range(self, node):
        return 0 < node and node < len(self.tree)

    '''
    Gets the range represented by the node in the tree. For leaf nodes `n`,
    returns `(n, n)`.

    _get_range :: int -> (int, int)
    '''
    def _get_range(self, node):
        if node not in self._range_cache:
            l = self._get_l(node)
            r = self._get_r(node)

            l_range = (node, node)
            if self._in_range(l):
                l_range = self._get_range(l)

            r_range = (node, node)
            if self._in_range(r):
                r_range = self._get_range(r)

            self._range_cache[node] = (l_range[0], r_range[1])
        return self._range_cache[node]

    '''
    Computes `f` (see ctor) across the range [l, r] (note inclusiveness).
    Computes recursively, so parametrize `SegTree`s with `f`s that make sense
    (e.g. `min` or `sum`).

    f_across_range :: int -> int -> a # same `a` as `f` from the ctor
    '''
    def f_across_range(self, l, r):
        root = 1
        return self._f_across_range(\
            self._get_leaf_offset() + l,\
            self._get_leaf_offset() + r,\
            root\
        )

    '''
    (wrapped by f_across_range)

    _f_across_range :: int -> int -> int -> a # same `a` as `f` from the ctor
    '''
    def _f_across_range(self, l, r, curr):
        (cl, cr) = self._get_range(curr)

        if l <= cl and cr <= r:
            # `curr` entirely within `(l, r)`
            return self.tree[curr]

        if r < cl or cr < l:
            # `(l, r)` has no overlap with `curr`
            return None

        # ranges partially overlap
        return self._f(\
            self._f_across_range(l, r, self._get_l(curr)),\
            self._f_across_range(l, r, self._get_r(curr))\
        )

    '''
    (for debugging)

    Represents `self` as a string
    '''
    def __repr__(self):
        return str(zip([i for i in xrange(0, len(self.tree))], self.tree))

def minimum(a, b):
    if a is not None and b is not None:
        return min(a, b)
    elif a is not None:
        return a
    elif b is not None:
        return b
    else:
        return None

def add(a, b):
    if a is not None and b is not None:
        return a + b
    elif a is not None:
        return a
    elif b is not None:
        return b
    else:
        return 0

def main():
    min_segtree = SegTree([2,5,1,4,9,3], minimum)
    sum_segtree = SegTree([2,5,1,4,9,3], add)

    print min_segtree.f_across_range(3, 6)
    print sum_segtree.f_across_range(3, 6)

if __name__ == '__main__':
    main()

/*
 * dynamic-range-tree
 *
 * Brett Wines
 *
 * December 2014
 */

#include <iostream>
#include <fstream>
#include <vector>
#include <map>
#include <set>
#include <algorithm>
#include <stdio.h>
#include <stdlib.h>

using namespace std;

/* A dynamic range tree is a data structure that supports two main operations:
 * querying the maximum or minimum values in the a contiguous range, and
 * incrementing all values in a contiguous range. This tree supports those
 * operations, but operates not on an array, but a B-tree. Updates and
 * queries are performed not on ranges, but on subtrees.
 */
class DynamicRangeTree {
    /* Implementation:
     *
     * A range tree is a binary tree where each node represents an interval
     * (leaf nodes represent degenerate intervals), which encloses the all
     * its children's intervals. Of course, there may not be a node whose
     * range is equal to that being queried or updated; in the general case,
     * there are two root-NULL paths that define the barriers enclosing the
     * interval in question. We get these with `subtrees_spanning_range_l_side`
     * and `subtrees_spanning_range_r_side`. When a raise occurs, we update
     * all these nodes's l or r children (respectively) marking that they
     * and all their children have had raises applied to them.
     */
public:
    /* Constuctor.
     *
     * `uid`s are 1-indexed. `values` is a 1-indexed vector of length `n+1`
     * of values to be stored in nodes. That is, `values[4]` is the value to
     * be stored in the node with uid `4`. `parents` is a *1-indexed* vector
     * of length `n+1` of parent uuids; this is so that uids align in a
     * semantically pleasing way. For example, if `parents[3] == 5`,
     * then the parent of node 3 is node 5.
     *
     * The node with uid 1 is to be the root of the tree; thus, parents[1]
     * is ignored and can be anything.
     */
    DynamicRangeTree(vector<int> parents, vector<int> values) {
        this->btree = BTree(parents, values);
        
        int lb = 1;
        int ub = (int)btree.size();
        root = construct_tree(make_pair(lb, ub));
    }
    
    /* O(log n) The maximum value in the the sub-tree rooted at
     * the b-tree node with the given uid.
     */
    int max_in_subtree(int root_uid) {
        SpanningInfo subtrees = subtrees_for_btree_subtree(root_uid);
        return max_in_range(subtrees);
    }

    /* O(log n) The minimum value in the the sub-tree rooted at
     * the b-tree node with the given uid.
     */
    int min_in_subtree(int root_uid) {
        SpanningInfo subtrees = subtrees_for_btree_subtree(root_uid);
        return min_in_range(subtrees);
    }

    /* O(log n) Updates every node in the sub-tree rooted at the b-tree
     * node with the given uid to have its value incremented by the
     * given amount. */
    void update_range(int root_uid, int increment) {
        SpanningInfo subtrees = subtrees_for_btree_subtree(root_uid);
        for (auto it=subtrees.begin(); it!=subtrees.end(); it++) {
            RangeTreeNode* subtree = it->first;
            increment_nodes_in_subtree(subtree, increment);
        }
    }
    
private:
    typedef pair<int, int> Range;
    
    /* A tree where each node can have arbitrarily many children.
     */
    class BTree {
    public:
        /* Default constructor. Do not call.
         */
        BTree() {}
        
        /* Constuctor.
         *
         * `uid`s are 1-indexed. `amounts` is a 1-indexed vector of length `n+1`
         * of values to be stored in nodes. That is, `amounts[4]` is the value
         * to be stored in the node with uid `4`. `parents` is a *1-indexed*
         * vector of length `n+1` of parent uuids; this is so that uids align in
         * a semantically pleasing way. For example, if `parents[3] == 5`,
         * then the parent of node 3 is node 5.
         *
         * The node with uid 1 is to be the root of the tree; thus, parents[1]
         * is ignored and can be anything.
         */
        BTree(vector<int> parents, vector<int> amounts) {
            int n_nodes = (int)amounts.size() - 1; // -1 for the 1-indexing
            if ((n_nodes == 0) || (parents.size() != n_nodes + 1)) {
                throw invalid_argument("Lengths and values of input "
                                       "vectors don't fit input "
                                       "specification defined in "
                                       "documentation.");
            }
            
            for (int uid=1; uid<=n_nodes; uid++) { // 1-indexing
                BTreeNode* node = new BTreeNode(amounts[uid], uid);
                uids_to_nodes[uid] = node;
            }
            
            // 2  = 1-indexing + node with uid 1 has no parent, so we skip it.
            for (int uid=2; uid<=n_nodes; uid++) {
                BTreeNode* child = uids_to_nodes[uid];
                BTreeNode* parent = uids_to_nodes[parents[uid]];
                parent->children.push_back(child);
            }
            
            assign_preorder_order();
        }
        
        /* A range representing the max and min preorder_orders
         * of nodes in the subtree rooted at `uid`. */
        Range preorder_range_of_subtree(int uid) {
            BTreeNode* node = uids_to_nodes[uid];
            if (is_leaf_node(node)) {
                // Leaf node ranges are degenerate
                return make_pair(node->preorder_order,
                                 node->preorder_order);
            } else {
                return make_pair(leftmost_decendant(node)->preorder_order,
                                 node->preorder_order);
            }
        }
        
        /* The number of nodes in the tree.
         */
        int size() {
            return (int)uids_to_nodes.size();
        }
        
        /* Gets the values of all the nodes in the tree, sorted in
         * pre-order order.
         */
        vector<int> node_values_in_preorder_order() {
            vector<BTreeNode*> all_nodes = map_values(uids_to_nodes);
            sort(all_nodes.begin(), all_nodes.end(), [](BTreeNode* a,
                                                        BTreeNode* b) {
                return a->preorder_order < b->preorder_order;
            });
            vector<int> values_in_preorder_order;
            for (int i=0; i<all_nodes.size(); i++) {
                values_in_preorder_order.push_back(all_nodes[i]->value);
            }
            return values_in_preorder_order;
        }
        
    private:
        /* A BTree node. Can be internal or a leaf node.
         */
        struct BTreeNode {
            int value;
            int uid;
            int preorder_order;
            vector<BTreeNode*> children;
            
            // Constructor
            BTreeNode(int value, int uid) {
                this->value = value;
                this->uid = uid;
                
                // Initialized in the `BTree` constructor -- can't
                // be initialized here because it's dependent on
                // other `BTreeNode`s.
                preorder_order = -1;
            }
        };
        
        /* An internal mapping for convenience and performance reasons.
         */
        map<int, BTreeNode*> uids_to_nodes;
        
        /* The root of the tree.
         */
        BTreeNode* root() {
            return uids_to_nodes[1];
        }
        
        /* Gets all the values stored in a STL map.
         */
        template <typename K, typename V>
        vector<V> map_values(map<K, V>& m) {
            vector<V> values;
            for (typename map<K, V>::const_iterator it=m.begin();
                 it!=m.end();
                 it++)
            {
                values.push_back(it->second);
            }
            return values;
        }
        
        /* Wrapped by `assign_preorder_order`. Do not call.
         * `curr` is to be initialized with the root of the
         * tree and `preorder_order` to 1. This function
         * assigns a value to the `preorder_order` field on
         * each node in the tree in the order of a pre-order
         * traversal. */
        void assign_preorder_order_rec(BTreeNode* curr, int& preorder_order) {
            for (int i=0; i<curr->children.size(); i++) {
                assign_preorder_order_rec(curr->children[i], preorder_order);
            }
            curr->preorder_order = preorder_order;
            preorder_order++;
        }
        
        /* Assigns a value to the `preorder_order` field on
         * each node in the tree in the order of a pre-order
         * traversal. */
        void assign_preorder_order() {
            int preorder_order = 1;
            assign_preorder_order_rec(root(), preorder_order);
        }
        
        /* Determines whether the given node is a leaf node
         * or not.
         */
        bool is_leaf_node(BTreeNode* node) {
            return node->children.size() == 0;
        }
        
        /* Gets the leftmost decendant of `curr`. If `curr`
         * is `NULL`, returns `NULL`.
         */
        BTreeNode* leftmost_decendant(BTreeNode* curr) {
            if (curr == NULL) {
                return NULL;
            }
            if (is_leaf_node(curr)) {
                return curr;
            }
            // [0] is leftmost child
            return leftmost_decendant(curr->children[0]);
        }
    };
    
    /* A node in a RangeTree. Can be an internal node or a leaf node.
     */
    struct RangeTreeNode {
        int max;   // max value in tree rooted at this node
        int min;   // min value in tree rooted at this node
        int raise; // raise for tree rooted at this node
        
        // The range of the tree rooted at this node
        int lb;
        int ub;
        
        // Left child, right child, and parent pointers, respectively.
        RangeTreeNode* l;
        RangeTreeNode* r;
        RangeTreeNode* parent;
        
        /* Constructor
         */
        RangeTreeNode(int lb, int ub) {
            this->lb = lb;
            this->ub = ub;
            
            l = NULL;
            r = NULL;
            parent = NULL;
            
            raise = 0;
        }
    };
    
    // Pairs of <subtree, all increments affecting this subtree>.
    typedef set<pair<RangeTreeNode*, int>> SpanningInfo;
    
    // The tree specified by the input to the tree.
    BTree btree;
    
    // The root of the RangeTree.
    RangeTreeNode* root;
    
    /* O(log n) Sets fields such that subsequent queries
     * to any node in the subtree reflect `increment`.
     */
    void increment_nodes_in_subtree(RangeTreeNode* subtree, int increment) {
        if (!subtree) {
            return;
        }
        
        // Every node in our subtree has had `increment`
        // applied to it, so our `min` and `max` values
        // increase accordingly.
        subtree->min += increment;
        subtree->max += increment;
        
        // TODO: comment why need to update
        // children instead of node itself
        increment_node(subtree->l, increment);
        increment_node(subtree->r, increment);
        
        // Parents need to know that the the max and min
        // amounts in their subtrees might have changed
        update_parents(subtree->parent, increment);
    }
    
    // If values change, parents of nodes with the changed
    // values need to know that the max and min amounts
    // in their subtrees might have changed.
    void update_parents(RangeTreeNode* curr, int raise) {
        if (curr == NULL) {
            return;
        }
        
        update_min_and_max(curr, curr->min, curr->max);
        
        update_parents(curr->parent, raise);
    }
    
    /* O(1) Increments the values in `node` by `increment`.
     */
    void increment_node(RangeTreeNode* node, int increment) {
        if (node) {
            if (is_leaf_node(node)) {
                // Implementation detail: `raise` is only
                // observed when descending through a node,
                // so for leaf nodes we need to update the
                // `min` and `max` fields instead.
                node->min += increment;
                node->max += increment;
            } else {
                node->raise += increment;
            }
        }
    }
    
    /* O(1) Determines whether the given node is a leaf node or not.
     */
    bool is_leaf_node(RangeTreeNode* node) {
        return (node != NULL) // NULL is not a leaf node
            && (node->l == NULL)
            && (node->r == NULL);
    }
    
    /* O(log n) Finds the DynamicRangeTree subtrees who make up the B-tree
     * subtree rooted at `root_uuid`. Returns a `SpanningInfo` to give the
     * information of the increments applied to each subtree.
     */
    SpanningInfo subtrees_for_btree_subtree(int root_uid) {
        Range range = btree.preorder_range_of_subtree(root_uid);
        return subtrees_spanning_range(range);
    }
    
    /* O(log n) Finds the subtrees whose ranges whose partition `range`.
     * Returns a `SpanningInfo` to give the information of the increments
     * applied to each subtree.
     */
    SpanningInfo subtrees_spanning_range(Range range) {
        SpanningInfo subtrees;
        subtrees_spanning_range_l_side(range, root, subtrees);
        subtrees_spanning_range_r_side(range, root, subtrees);
        return subtrees;
    }
    
    /* O(log n) Symmetric to `subtrees_spanning_range_l_side`.
     * Fills `subtrees` with left children of nodes along the
     * path of the root-leaf path to the rightmost node in the
     * given range. The reason we fill with children is if we
     * fill with nodes along the path itself, these nodes will
     * span a range greater than the given range because they
     * have children on the opposite side who represent an area
     * outside the range.
     *
     * If you update this function, make sure to update its
     * sister function. It's cleaner in this case to keep them
     * separate, otherwise the code'd just be shared.
     */
    void subtrees_spanning_range_r_side(Range range,
                                        RangeTreeNode* curr,
                                        SpanningInfo& subtrees,
                                        int raise=0) {
        if (curr == NULL) {
            return;
        }
        
        // Track all raises applied to any nodes in the range
        raise += curr->raise;
        
        if (encloses(range, curr)) {
            subtrees.insert(make_pair(curr, raise));
            // If the current node is enclosed by the range,
            // descending and adding children nodes would be
            // redundant (or excessive if we didn't add this
            // node).
            return;
        }
        
        RangeTreeNode* next;
        if (!curr->r || (curr->r->lb > range.second)) {
            // range does not lie in r subtree of curr_r
            next = curr->l;
        } else {
            next = curr->r;
        }
        
        // `curr`'s child might span the range, but `next`
        // might not be this child; i.e. `curr` might like
        // along the spine of the root-leaf path. Drawing
        // pictures here is helpful.
        if (encloses(range, curr->l) && next != curr->l) {
            subtrees.insert(make_pair(curr->l, raise));
        }
        
        subtrees_spanning_range_r_side(range, next, subtrees, raise);
    }
    
    /* O(log n) Symmetric to `subtrees_spanning_range_r_side`.
     * Fills `subtrees` with right children of nodes along the
     * path of the root-leaf path to the leftmost node in the
     * given range. The reason we fill with children is if we
     * fill with nodes along the path itself, these nodes will
     * span a range greater than the given range because they
     * have children on the opposite side who represent an area
     * outside the range.
     *
     * If you update this function, make sure to update its
     * sister function. It's cleaner in this case to keep them
     * separate, otherwise the code'd just be shared.
     */
    void subtrees_spanning_range_l_side(Range range,
                                        RangeTreeNode* curr,
                                        SpanningInfo& subtrees,
                                        int raise=0) {
        if (curr == NULL) {
            return;
        }
        
        // Track all raises applied to any nodes in the range
        raise += curr->raise;
        
        if (encloses(range, curr)) {
            subtrees.insert(make_pair(curr, raise));
            // If the current node is enclosed by the range,
            // descending and adding children nodes would be
            // redundant (or excessive if we didn't add this
            // node).
            return;
        }
        
        RangeTreeNode* next;
        if (!curr->l || (curr->l->ub < range.first)) {
            // range does not lie in l subtree of curr_l
            next = curr->r;
        } else {
            // some of range is in l subtree of curr_l
            next = curr->l;
        }
        
        // `curr`'s child might span the range, but `next`
        // might not be this child; i.e. `curr` might like
        // along the spine of the root-leaf path. Drawing
        // pictures here is helpful.
        if (encloses(range, curr->r) && next != curr->r) {
            subtrees.insert(make_pair(curr->r, raise));
        }
        
        subtrees_spanning_range_l_side(range, next, subtrees, raise);
    }
    
    /* O(1) Determines whether a node lies entirely within the
     * given range (boundaries are included)
     */
    bool encloses(Range range, RangeTreeNode* node) {
        return (node != NULL)
        && (node->lb >= range.first)
        && (node->ub <= range.second);
    }
    
    /* O(n) Identifies the "best" amount within a range, where
     * "best" is determined by the latter two parameters.*/
    int best_in_range(SpanningInfo& subtrees,
                      bool (*should_update_best)(RangeTreeNode*, int, int),
                      int (*update_best)(RangeTreeNode*, int))
    {
        if (subtrees.size() == 0) {
            throw invalid_argument("No subtrees");
        }
        int best = -1;
        bool is_first_iteration = true;
        for (auto it=subtrees.begin(); it!=subtrees.end(); it++) {
            RangeTreeNode* subtree = it->first;
            int raise = it->second;
            
            if (should_update_best(subtree, raise, best) ||
                is_first_iteration)
            {
                best = update_best(subtree, raise);
                is_first_iteration = false;
            }
        }
        return best;
    }
    
    /* O(n), where `n` is the number of subtrees (bounded by
     * log N, where N is the size of the range / number of
     * leaf nodes in the tree). Returns the maximum element
     * out of all of the specified subtrees.
     */
    int max_in_range(SpanningInfo& subtrees) {
        return best_in_range(subtrees,
                             [](RangeTreeNode* subtree, int raise, int max) {
                                 return subtree->max + raise > max;
                             },
                             [](RangeTreeNode* subtree, int raise) {
                                 return subtree->max + raise;
                             });
    }
    
    /* O(n), where `n` is the number of subtrees (bounded by
     * log N, where N is the size of the range / number of
     * leaf nodes in the tree). Returns the minimum element
     * out of all of the specified subtrees.
     */
    int min_in_range(SpanningInfo& subtrees) {
        return best_in_range(subtrees,
                             [](RangeTreeNode* subtree, int raise, int min) {
                                 return subtree->min + raise < min;
                             },
                             [](RangeTreeNode* subtree, int raise) {
                                 return subtree->min + raise;
                             });
    }
    
    /* O(1) Determines whether `range` is to be represented
     * with a leaf node, as opposed to with an internal node.
     */
    bool range_corresponds_to_leaf_node(Range range) {
        return range.first == range.second;
    }
    
    /* O(n), where `n` is the number of elements in the tree.
     * Returns the root of the constructed tree. `range`
     * is the range of the entire tree.
     */
    RangeTreeNode* construct_tree(Range range) {
        int leaf = 0;
        vector<int> node_values_in_preorder_order
        = btree.node_values_in_preorder_order();
        return construct_tree(range, leaf, node_values_in_preorder_order);
    }
    
    /* (wrapped by `construct_tree(Range)`; do not call)
     * O(n), where `n` is the number of elements in the tree.
     * Returns the root of the constructed RangeTree. `range`
     * is the range of the entire tree. `leaf` is to be
     * initialized to 0, for it represents how many leaf
     * nodes have been constructed in the tree of recursive
     * calls this function makes.
     * */
    RangeTreeNode* construct_tree(Range range,
                                  int& leaf,
                                  vector<int>& node_values_in_preorder_order)
    {
        int lb = range.first;
        int ub = range.second;
        if (range_corresponds_to_leaf_node(range)) {
            RangeTreeNode* node = new RangeTreeNode(lb, ub);
            node->max = node_values_in_preorder_order[leaf];
            node->min = node_values_in_preorder_order[leaf];
            leaf++;
            return node;
        }
        if (lb > ub) {
            return NULL;
        }
        
        int mid = lb + (ub - lb) / 2;
        
        RangeTreeNode* node = new RangeTreeNode(lb, ub);
        node->l = construct_tree(make_pair(lb, mid),
                                 leaf,
                                 node_values_in_preorder_order);
        node->r = construct_tree(make_pair(mid+1, ub),
                                 leaf,
                                 node_values_in_preorder_order);
        if (node->l) node->l->parent = node;
        if (node->r) node->r->parent = node;
        
        update_min_and_max(node, 0, 0);
        if (is_leaf_node(node)) {
            // We shouldn't ever get here (in theory), but
            // integer arithmetic is messy to check and error-prone,
            // so better to be on the safe side.
            node->min = node_values_in_preorder_order[leaf];
            node->max = node_values_in_preorder_order[leaf];
            leaf++;
        }
        
        return node;
    }
    
    /* O(1) Updates `node`'s `min` and `max` fields to
     * the min and max values in the range rooted at it.
     */
    void update_min_and_max(RangeTreeNode* node,
                            int default_min_value,
                            int default_max_value) {
        if (node->l != NULL) {
            if (node->r != NULL) {
                node->min = min(node->l->min, node->r->min);
                node->max = max(node->l->max, node->r->max);
            } else {
                node->min = node->l->min;
                node->max = node->l->max;
            }
        } else {
            if (node->r != NULL) {
                node->min = node->r->min;
                node->max = node->r->max;
            } else {
                node->min = default_min_value;
                node->max = default_max_value;
            }
        }
    }
};

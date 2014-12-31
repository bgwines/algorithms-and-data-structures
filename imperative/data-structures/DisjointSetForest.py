

class DisjointSetForest(object):
	"""
	A disjoint-set forest is a partitioning of a set that 
	supports `union` (merge two sets) and `find` (find the 
	representative of the set in which an element is) in log* 
	time. 

	The data structure is attributed to McIlroy and Morris in
	the 70s, but its complexity was first analyzed by Tarjan,
	many years later.
	"""
	def __init__(self, elems):
		self.elems = elems

		self.depths = dict()
		self.roots = dict()
		for elem in elems:
			# all elements are in singleton sets before any unioning
			self.depths[elem] = 1
			self.roots[elem] = elem

	"""
	Union(x, y) - merges the sets in which `x` and `y` reside.
	"""
	def union(self, x, y):
		# only care about operating on roots of x's and y's 
		# respective trees
		x = self.find(x)
		y = self.find(y)

		if x == y:
			# part of same set; no operations needed
			return

		if self.depths[x] > self.depths[y]:
			# make sure to not increase height of
			# tree during  merging if possible
			temp = x
			x = y
			y = temp

		if self.depths[x] == self.depths[y]:
			# only time tree depth increases; any other time we 
			# make the smaller one a child of the larger one
			self.depths[y] += 1
		
		# merge the trees. calling root() on elements in x's 
		# tree will still return `x`, but they will get updated 
		# during `find`s, and calling more `union`s in the mean-
		# time will not have any negative effect because `find` 
		# is called on the elements the sets of which are being 
		# merged.
		self.roots[x] = y

	"""
	Find(x) - returns the representative of the set in which `x` 
			  resides.
	"""
	def find(self, x):
		if self.roots[self.roots[x]] != self.roots[x]:
			# if our "root", intended to be the representative,
			# is not actually a root, we update it.
			self.roots[x] = self.find(self.roots[x])

		return self.roots[x]

	def __str__(self):
		return "sets: " + str(self.sets())
			+ "\ndepths: " + str(self.depths.values())
			+ "\nfinds: " + str([self.find(e) for e in self.elems])

	def sets(self):
		map(self.find, self.elems)
		sets = dict()
		for elem in self.elems:
			if self.roots[elem] not in sets:
				sets[self.roots[elem]] = []
			sets[self.roots[elem]].append(elem)
		return sets.values()	

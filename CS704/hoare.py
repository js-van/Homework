class HPL:	
	def __init__(self, s, store, children = []):
		self.symbol = s
		self.children = children
		self.store = store

	def replace(self, symbol, npred):
		for c in self.children:
			if(c.symbol == symbol):
				self.children[c] = npred
			else:
				c.replace(symbol, npred)
	
	def __str__(self):
		if( self.symbol[0] == '\\' ):
			return '\\left ( ' + str(self.children[0])+ ' ' + self.symbol + ' ' + str(self.children[1]) + '\\right )'
		elif( self.symbol == 'store' ):
			return str(self.store) + '\\left ( ' + str(self.children[0]) + '\\right )'
		else:
			return self.symbol

class Store:
	def __init__(self):
		self.store = []
	
	def subst(self, symbol, npred):
		for (k, v) in self.store:
			if(k == symbol):
				v = npred
			else
				v.replace(symbol, npred)
	
	def __str__(self):
		kstr = ''
		for (k, v) in self.store.iteritems():
			if(len(kstr) > 0):
				kstr += ', '
			kstr += str(k) + ' \\mapsto '	+ str(v)
		if(len(kstr) > 0):
			kstr = '\\left [ ' + kstr + ' \\right ] '
		return 'F_\\rho ' + kstr
	
	def lookup(self, expr):
		if(expr.symbol == 'store'):
			expr = expr.store.lookup(expr.children[0])
		if(expr.symbol[0] == '\\'):
			expr = HPL(expr.symbol, expr.store, map(self.lookup, expr.children))
		for (k,v) in self.store:
			if(k == expr):
				return v
		return HPL('store', self, [expr])


init_store = Store()
X = HPL('c_X', init_store)
Y = HPL('c_Y', init_store)
pX = HPL('c_{\\&px}', init_store)
pY = HPL('c_{\\&px}', init_store)
temp = HPL('c_{temp}', init_store)


def binop(op, A, B):
	return HPL(op, init_store, children=[A,B])

def deref(A, store):
	return store.lookup(store.lookup(A]))

init_hpl = binop('\\vee', binop('\\neq', X, deref(pX, init_store)), binop('\\neq', Y, deref(pY, init_store)))


def val_ptr_assign(nv, ptr):
	init_hpl.replace(nv, lookup(lookup(ptr)))

def ptr_ptr_assign(pA, pB):
	return
	

print '\\documentclass{article}'
print '\\begin{document}'
print '\\['
print init_hpl
print '\\]'
print '\\end{document}'


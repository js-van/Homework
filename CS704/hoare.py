import copy

store = None
hpl = None

class HPL:	
	def __init__(self, s, children = [], st=None):
		self.symbol = s
		self.children = children
		if(st is None):
			self.store = None
		else:
			self.store = copy.deepcopy(st)

	def replace(self, s, npred):
		for (i,c) in enumerate(self.children):
			if(c == s):
				self.children[i] = npred
			else:
				c.replace(s, npred)
	
	def __str__(self):
		if( self.symbol[0] == '\\' ):
			return ' \\left ( ' + str(self.children[0])+ ' ' + self.symbol + ' ' + str(self.children[1]) + '\\right )'
		elif( self.symbol == 'store' ):
			return ' F_{\\rho} \\left ( ' + str(self.children[0]) + ' \\right ) '
		else:
			return self.symbol
			
	def __eq__(self, other):
		return self.symbol == other.symbol and self.children == other.children

class Store:
	def __init__(self):
		self.store = []
		
	def replace(self, s, npred):
		#self.store = copy.deepcopy(self.store)
		for i in range(len(self.store)):
			if(self.store[i][0] == s):
				self.store[i] = (npred, self.store[i][1])
			self.store[i][0].replace(s, npred)
			if(self.store[i][1] == s):
				self.store[i] = (self.store[i][0], npred)
			self.store[i][1].replace(s, npred)
			
	def subst(self, s, npred):
		self.store = copy.deepcopy(self.store)
		in_s = False
		for (k, v) in self.store:
			if(k == s):
				v = npred
				in_s = True
			else:
				v.replace(s, npred)			
		if(not in_s):
			self.store.append( (s, npred) )
			
	def deref(self,A):
		return HPL('store', [A], self)
	
	def __str__(self):
		kstr = ''
		for (k, v) in self.store:
			if(len(kstr) > 0):
				kstr += ', '
			kstr += str(k) + ' \\mapsto '	
			kstr += str(v)
		if(len(kstr) > 0):
			kstr = '\\left [ ' + kstr + ' \\right ] '
		return 'F_\\rho ' + kstr
	
def binop(op, A, B):
	return HPL(op, [A,B])

def deref(A):
	return store.deref(A)
	
def ddref(A):
	return deref(deref(A))

def assign(s, v):
	hpl.replace(s, v)
	store.replace(s, v)
	
def ptr_assign(s, v):
	global hpl, store
	ptr = ddref(s)
	hpl.replace(ptr, v)
	store.subst(ptr, v)

def fptr_assign(s, v):
	global hpl, store
	ptr = HPL('store', [s], store)
	hpl.replace(deref(ptr), v)
	store.subst(ptr, v)

stepno = 0
def print_cmd():
	global stepno
	stepno += 1
	print str(stepno) + ' & '
	print '$' + str(hpl) + '$'
	print ' & '
	print '$' + str(store) + '$'
	print ' \\\\'


store = Store()
X = HPL('c_X')
Y = HPL('c_Y')
pX = HPL('c_{\\&px}')
pY = HPL('c_{\\&py}')
temp = HPL('c_{temp}')
hpl = binop('\\vee', binop('\\neq', X, ddref(pY)), binop('\\neq', Y, ddref(pX)))
	
print '\\documentclass{article}'
print '\\usepackage{rotating}'
print '\\begin{document}'
print '\\begin{sidewaystable} \centering \\begin{tabular}{|c|c|c|} \hline Step & Condition & Store \\\\ \hline'
print_cmd()

fptr_assign(pY, temp)
print_cmd()

fptr_assign(pX, ddref(pY))
print_cmd()

assign(temp, ddref(pX))
print_cmd()

assign(Y, ddref(pY))
print_cmd()

assign(X, ddref(pX))
print_cmd()

print '\hline \\end{tabular} \\end{sidewaystable}'
print '\\end{document}'


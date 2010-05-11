#include <cstdio>
#include <vector>

using namespace std;

void DotProduct_gen(vector<int> x)
{
	printf("int DotProduct_sp(vector<int> y) { return ");	
	if(x.size() == 0)
		printf("0");
	for(int i=0; i<x.size(); i++)
	{
		if(i > 0)
			printf("+");
		printf("%d*y[%d]", x[i], i); 
	}
	printf("; }");
}

main()
{
	vector<int> x;
	DotProduct_gen(x);
}

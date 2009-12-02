#include <stdio.h>

// Dette c-eksempel beregner det samme som encrypt.jan

int main() {
	int x,y;
	int key;

	scanf("%d", &key);
	scanf("%d", &x);

	for(y = 0; y < 19; y++) {
		if(x % 2 == 0) {
			x/=2;
		} else {
			x/=2;
			x+=key;
		}
	}
		

	printf("%d\n", key);
	printf("%d\n", x);
}

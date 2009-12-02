#include <stdio.h>

// Dette c-eksempel beregner det samme som decrypt.jan

int main() {
	int x,y;
	int key;

	scanf("%d", &key);
	scanf("%d", &x);

	for(y = 0; y < 19; y++) {
		if(x >= key) {
			x -= key;
			x *= 2;
			x++;
		} else {
			x *= 2;
		}
	}
		

	printf("%d\n", key);
	printf("%d\n", x);
}

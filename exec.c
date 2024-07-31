#include <stdio.h>
#include <stdlib.h>
#include <sys/mman.h>

void ree(int a) {
	printf("value = %d\n", a);
}


int main() {
	FILE* f = fopen("./code", "r");

	fseek(f, 0, SEEK_END);
	int flen = ftell(f);
	rewind(f);

	char* buf;
	posix_memalign((void **)&buf, 4096, flen);
	mprotect(buf, flen, PROT_EXEC | PROT_READ | PROT_WRITE);

	fread(buf, flen, 1, f);

	printf("Contents: ");
	for (int i = 0; i < flen; i++) {
		printf("%c", buf[i]);
	}


	printf("\n");
	ree(1);

	fflush(stdout);
	
	int (*fn)(void (*)(int)) = (int (*)(void (*)(int)))buf;
	fn(ree);

	return 0;
}

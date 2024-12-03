#include <stdio.h>

int main() {
	int c;
	int lhs, rhs;
	int result;

	result = 0;

START:
	if ((c = getchar()) == EOF)
		goto END;

	switch (c)
	{
	case 'm':
		goto HANDLE_M;
	}
	goto START;

HANDLE_M:
	printf("HANDLE_M\n");
	if ((c = getchar()) == EOF)
		goto END;

	switch (c)
	{
	case 'u':
		goto HANDLE_U;
	}
	goto START;

HANDLE_U:
	printf("HANDLE_U\n");
	if ((c = getchar()) == EOF)
		goto END;

	switch (c)
	{
	case 'l':
		goto HANDLE_L;
	}
	goto START;

HANDLE_L:
	printf("HANDLE_L\n");
	if ((c = getchar()) == EOF)
		goto END;

	switch (c)
	{
	case '(':
		goto HANDLE_LEFTPAREN;
	}
	goto START;
	
HANDLE_LEFTPAREN:
	printf("HANDLE_LEFTPAREN\n");
	if ((c = getchar()) == EOF)
		goto END;

	if ('0' <= c && c <= '9')
		goto HANDLE_LHS;
	goto START;

HANDLE_LHS:
	printf("HANDLE_LHS\n");

	lhs = 0;
	while ('0' <= c && c <= '9') {
		lhs = lhs * 10 + c - '0';
		c = getchar();
	}

	switch (c)
	{
	case ',':
		goto HANDLE_COMMA;
	case EOF:
		goto END;
	}
	goto START;

HANDLE_COMMA:
	printf("HANDLE_COMMA\n");
	printf("lhs = %d\n", lhs);

	if ((c = getchar()) == EOF)
		goto END;

	if ('0' <= c && c <= '9')
		goto HANDLE_RHS;
	goto START;

HANDLE_RHS:
	printf("HANDLE_RHS\n");

	rhs = 0;
	while ('0' <= c && c <= '9') {
		rhs = rhs * 10 + c - '0';
		c = getchar();
	}

	switch (c)
	{
	case ')':
		goto HANDLE_RIGHTPAREN;
	case EOF:
		goto END;
	}
	goto START;

HANDLE_RIGHTPAREN:
	printf("HANDLE_RIGHTPAREN\n");
	printf("rhs = %d\n", rhs);

	result += lhs * rhs;

	goto START;

END:
	printf("%d\n", result);
	return 0;
}

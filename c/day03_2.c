#include <stdio.h>

int main() {
	int c;
	int lhs, rhs;
	int result;
	int enabled;

	result = 0;
	enabled = 1;

START:
	if ((c = getchar()) == EOF)
		goto END;

	switch (c)
	{
	case 'm':
		goto HANDLE_M;
	case 'd':
		goto HANDLE_D;
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

	ungetc(c, stdin);
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

	ungetc(c, stdin);
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

	ungetc(c, stdin);
	goto START;
	
HANDLE_LEFTPAREN:
	printf("HANDLE_LEFTPAREN\n");
	if ((c = getchar()) == EOF)
		goto END;

	if ('0' <= c && c <= '9')
		goto HANDLE_LHS;

	ungetc(c, stdin);
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
	}

	ungetc(c, stdin);
	goto START;

HANDLE_COMMA:
	printf("HANDLE_COMMA\n");
	printf("lhs = %d\n", lhs);

	if ((c = getchar()) == EOF)
		goto END;

	if ('0' <= c && c <= '9')
		goto HANDLE_RHS;

	ungetc(c, stdin);
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
		goto HANDLE_RPAREN;
	}

	ungetc(c, stdin);
	goto START;

HANDLE_RPAREN:
	printf("HANDLE_RIGHTPAREN\n");
	printf("rhs = %d\n", rhs);

	if (enabled)
		result += lhs * rhs;

	goto START;

HANDLE_D:
	printf("HANDLE_D\n");

	if ((c = getchar()) == EOF)
		goto END;

	switch (c)
	{
	case 'o':
		goto HANDLE_O;
	}

	ungetc(c, stdin);
	goto START;

HANDLE_O:
	printf("HANDLE_O\n");

	if ((c = getchar()) == EOF)
		goto END;

	switch (c)
	{
	case 'n':
		goto HANDLE_N;
	case '(':
		goto HANDLE_DOLPAREN;
	}

	ungetc(c, stdin);
	goto START;

HANDLE_DOLPAREN:
	printf("HANDLE_DOLPAREN");

	if ((c = getchar()) == EOF)
		goto END;

	switch (c)
	{
	case ')':
		goto HANDLE_DORPAREN;
	}

	ungetc(c, stdin);
	goto START;

HANDLE_DORPAREN:
	printf("HANDLE_DORPAREN");

	enabled = 1;

	goto START;

HANDLE_N:
	printf("HANDLE_N\n");

	if ((c = getchar()) == EOF)
		goto END;

	switch (c)
	{
	case '\'':
		goto HANDLE_APO;
	}

	ungetc(c, stdin);
	goto START;

HANDLE_APO:
	printf("HANDLE_APO\n");

	if ((c = getchar()) == EOF)
		goto END;

	switch (c)
	{
	case 't':
		goto HANDLE_T;
	}

	ungetc(c, stdin);
	goto START;

HANDLE_T:
	printf("HANDLE_T\n");

	if ((c = getchar()) == EOF)
		goto END;

	switch (c)
	{
	case '(':
		goto HANDLE_DONTLPAREN;
	}

	ungetc(c, stdin);
	goto START;

HANDLE_DONTLPAREN:
	printf("HANDLE_DONTLPAREN");

	if ((c = getchar()) == EOF)
		goto END;

	switch (c)
	{
	case ')':
		goto HANDLE_DONTRPAREN;
	}

	ungetc(c, stdin);
	goto START;

HANDLE_DONTRPAREN:
	printf("HANDLE_DONTRPAREN");

	enabled = 0;

	goto START;

END:
	printf("%d\n", result);
	return 0;
}

void test_1() {
    int var = 5;
    int x = var;
}

void test_2() {
    void f(int a, void *b) {}
    f(3, NULL);
}

void test_3() {
    bool x = true;
    bool y = false;
    int *****z = NULL;
    int a = 7;
    int b = sizeof(bool*);
}

void test_4() {
    int a = +1;
    int b = -2;
    int c = !3;
    void ***x = NULL;
    void ***e = ++x;
    void ***f = --x;
    void ***g = x++;
    void ***h = x--;
}

void test_5() {
    int a = 1;
    int *b = &a;
    int c = *b;
}

void test_6() {
    int a = 1 + 2;
    int b = 1 - 2;
    int c = 1 * 2;
    int d = 1 / 2;
    int e = 1 % 2;
    int f = 1 && 2;
    int g = 1 || 2;
}

void test_7() {
    int a = 1;
    int *b = &a + 2;
    int *c = &a - 2;
}

void test_8() {
    int a = 1;
    int b = 2;
    int c = &a - &b;
}

int main() {
    return 0;
}

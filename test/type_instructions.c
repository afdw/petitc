void test_1() {
    int a = 4;
    int b;
    if (a) {
        b = 6;
    } else {
        b = 8;
    }
}

void test_2() {
    int a = 3;
    while (a < 5) {
        a = a * 2;
        if (a == 1) {
            break;
        }
        if (a == 7) {
            continue;
        }
        a = a * 4;
    }
}

int test_3() {
    int res = 0;
    for (int i = 0; i < 10; i++) {
        res = res + putchar(i);
    }
    return res;
}

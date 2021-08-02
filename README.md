# PAFunction
 Partial application function class in C++ with consistent typing. Works with (capturing) lambdas, function pointer, and member functions. Tested on a lot of cases, but if you do run into issues let me know. I am also over on discord at <code>@Kaixo#0001</code>
```cpp
using namespace kaixo;
int Add(int a, int b) { return a + b; }
struct Object{ int Add(int a, int b) { return a + b; } };
int main() 
{
    // Lambda
    PAFunction<int(int, int)> lambda = [](int a, int b) { return a + b; };
    PAFunction<int(int)> lambda2 = lambda(1);
    int lambdaResult = lambda2(3);
    assert(lambdaResult == 4);

    // Function Pointer
    PAFunction<int(int, int)> funPtr = Add;
    PAFunction<int(int)> funPtr2 = funPtr(1);
    int funPtrResult = funPtr2(3);
    assert(funPtrResult == 4);
    
    // Member function
    Object obj;
    PAFunction<int(int, int)> memberFun = { &Object::Add, obj };
    PAFunction<int(int)> memberFun2 = memberFun(1);
    int memberFunResult = memberFun2(3);
    assert(memberFunResult == 4);
}
```

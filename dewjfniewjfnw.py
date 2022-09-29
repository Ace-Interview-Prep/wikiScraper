
fmap (plusOne, range(0,9))

def solution(xs):
    # write your code in Python 3.6
    
    x, xs = uncons(range(0,20))

    print(x)
    print(xs)

# f should be capable of handling all Cardinalities of input, xs
# so ideally its a heterogeneus list 
def fmap(f, xs): 
    if len(xs) == 0: # case xs of [] 
        return xs
    else:
        x2,xs2 = uncons(xs)
        x2 = f(x2)
        return [x2] ++ fmap(f, xs2)
        

    
def uncons(xs): 
    if len(xs) != 0:
        x = xs.pop(0)
    return x, xs

import numpy as np

def iMSUB(i, j, n):
    return i + n * j

d = dict()
u = dict()
dwrk = 0
dwrk_w = 0
iwrk = 0

# NO need for cmeans_setup once it's all about allocating memory


def cmeans_sign(x):
    if x == 0:
        return 0
    ret_sign = 1 if x > 0 else -1
    return ret_sign

def cmeans_weighted_median(x, w, len):
    iwrk = x.argsort()
    x.sort()

    sum = 0

    dwrk = np.array

    for i in iwrk:
        dwrk = np.append(dwrk, w[i])
        sum = sum + w[i]

    for k in dwrk:
        w[k] = dwrk[k] / sum

    cumsum_w = 0
    cumsum_w_x = 0


    mval = float('inf')
    #prob in cmeans.c ln 80....
    marg = x

    for j in range(0, w.shape[0] - 1):
        cumsum_w = cumsum_w + w[j]
        cumsum_w_x = cumsum_w_x + (w[j] * x[j]);
        val = x[j] * (cumsum_w - .5) - cumsum_w_x;
        if (val < mval):
            marg = x[i]
            mval = val

    return marg


def ufcl_dissimilarities(x, p, nr_x, nc, nr_p, dist, ix): #the last parameters D is global on cmeans.c ln 113
    global d

    # d = dict()

    for ip in range(0, nr_p - 1):
        sum = 0
        for j in range(0, nc - 1):
            v = x[iMSUB(ix, j, nr_x)] - p[iMSUB(ip, j, nr_p)]
            if (dist == 0):
                sum = sum + (v * v)
            if (dist == 1):
                sum = abs(v)

        # print(d) #DEBUG
        d[iMSUB(ip, ix, nr_x)] = sum
        # print("sum")
        # print(iMSUB(ip, ix, nr_x))


def cmeans_dissimilarities(x, p, nr_x, nc, nr_p, dist): # again, the last param d is param and global (by ref in c++) not included
    global d

    for ix in range(0, nr_x - 1):
        ufcl_dissimilarities(x, p, nr_x, nc, nr_p, dist, ix)

        # print("break 84") # WORKED FOR NOW
        # print(d)

def ufcl_memberships(d, nr_x, nr_p, exponent, ix): # Argument u removed to comply with reference thingy cmeans.c ln 135
    n_of_zeroes = 0
    global u

    # print("break 89")
    # print(d)

    for ip in range(0, nr_p - 1):
        global u
        if(d.get(iMSUB(ix, ip, nr_x)) == None): d[iMSUB(ix, ip, nr_x)] = 0;

        if(d[iMSUB(ix, ip, nr_x)] == 0):
            n_of_zeroes = n_of_zeroes + 1

    if (n_of_zeroes > 0):
        v = 1 / n_of_zeroes
        for ip in range(0, nr_p - 1):
            if (d[iMSUB(ix, ip, nr_x)] == 0):
                vlr = v
            else:
                vlr = 0
            idx = ix + nr_x * ip
            global u
            u[idx] = vlr

    else:
        sum = 0
        for ip in range(0, nr_p - 1):
            v = 1 / pow(d[iMSUB(ix, ip, nr_x)], exponent)
            sum =  sum + v
            idx = ix + nr_x * ip
            u[idx] = v
        for ip in range(0, nr_p - 1):
            idx = ix + nr_x * ip
            u[idx] = u[idx] / sum
    #
    # print("u 120 UFCL_MEMBA")
    # print(u)

def cmeans_memberships(d, nr_x, nr_p, exponent):  # Argument u removed to comply with reference thingy cmeans.c ln 168
    global u



    # print("break 118")
    # print(d)

    for ix in range(0, nr_x -1):
        ufcl_memberships(d, nr_x, nr_p, exponent, ix)

    print("u 134 cmeans_memberships")
    print(u)

def cmeans_prototypes(x, y, w, nr_x, nc, nr_p, f, dist): # Argument p removed to comply with reference thingy cmeans.c ln 181
    global dwrk_x
    global dwrk_w

    if (dist == 0):
        for ip in range(0, nr_p - 1):
            for j in range(0, nc - 1):
                idx = ip + nr_p * j
                p[idx] = 0
                sum = 0
                for ix in range(0,nr_x - 1):
                    v = w[ix] * pow(u[iMSUB(ix, ip, nr_x)], f)
                    sum = sum + v
                    for j in range(0, nc - 1):
                        p[iMSUB(ip, j, nr_p)] = p[iMSUB(ip, j, nr_p)] + (v * MSUB(x, ix, j, nr_x))

                for j in range(0, nc - 1):
                    p[iMSUB(ip, j, nr_p)] = p[iMSUB(ip, j, nr_p)] / sum
    else:
        for ip in range(0, nr_p - 1):
            for j in range(0, nc -1):
                for ix in range(0, nr_x):
                    dwrk_x[ix] = x[iMSUB(ix, j, nr_x)];
                    dwrk_w[ix] = w[ix] * pow(u[iMSUB(ix, ip, nr_x)], f)
                p[iMSUB(ip, j, nr_p)] = cmeans_weighted_median(dwrk_x, dwrk_w, nr_x)

def cmeans_error_fn(u, d, w, nr_x, nr_p, f):
    print("u 164 cmeans_error_fn")
    print(u)
    sum = 0
    for ix in range(0, nr_x - 1):
        for ip in range(0, nr_p - 1):
            print("w")
            print(w)
            print("u")
            print(u)
            print("f")
            print(f)
            print("d")
            print(d)
            sum = sum + ( w[ix] * pow(u[iMSUB(ix, ip, nr_x)], f) * d[iMSUB(ix, ip, nr_x)])
    return sum

def cmeans(x,
           nr_x,
           nc,
           p,
           nr_p,
           w,
           f,
           dist,
           itermax,
           reltol,
           verbose,
           u_name,
           ermin,
           iter): # ermin seems passed by reference, need to check global scope
    exponent = 1 / (f - 1)


    cmeans_dissimilarities(x, p, nr_x, nc, nr_p, dist) # Check da d

    # print("break 179")
    # print(d)

    cmeans_memberships(d, nr_x, nr_p, exponent) # Check da u


    print("break 205 u")
    print(u)



    old_value = cmeans_error_fn(u, d, w, nr_x, nr_p, f)
    new_value = cmeans_error_fn(u, d, w, nr_x, nr_p, f)

    iter = 0

    while (iter < itermax):
        cmeans_prototypes(x, u, w, nr_x, nc, nr_p, f, dist);
        cmeans_dissimilarities(x, p, nr_x, nc, nr_p, dist);
        cmeans_memberships(d, nr_x, nr_p, exponent);
        new_value = cmeans_error_fn(u, d, w, nr_x, nr_p, f);

        if (abs(old_value - new_value) < (reltol * (old_value + reltol))):
            if verbose:
                print(f'Iteration {iter}, Error: {new_value}.')
                break;
        else:
            if verbose:
                ermin = cmeans_error_fn(u, d, w, nr_x, nr_p, f);
                print(f'Iteration {iter}, Error: {new_value}.')
            old_value = new_value;

        iter = iter + 1
    ermin = new_value


def ufcl_prototypes(x, u, w, nr_x, nc, nr_p, f, dist, lrate, ix): # p ommited, cmeans.c ln 274
    for ip in range(0, nr_p):
        for j in range(0, nc - 1):
            grad = x[iMSUB(ix, j, nr_x)] - p[iMSUB(ip, j, nr_p)]
            if (dist == 1):
                grad = cmeans_sign(grad)
                p[iMSUB(ip, j, nr_p)] = lrate * w[ix] * pow(u[iMSUB(ix, ip, nr_x)], f) * grad


def ufcl(x, nr_x, nc, p, nr_p, w, f, dist, itermax, reltol, verbose, rate_par, u, ermin, iter):
    exponent = 1 / (f - 1)

    cmeans_dissimilarities(x, p, nr_x, nc, nr_p, dist) # Check da d
    cmeans_memberships(d, nr_x, nr_p, exponent) # Check da u

    old_value = cmeans_error_fn(u, d, w, nr_x, nr_p, f)
    new_value = cmeans_error_fn(u, d, w, nr_x, nr_p, f)

    iter = 0

    while (iter < itermax):
        ufcl_dissimilarities(x, p, nr_x, nc, nr_p, dist);
        ufcl_memberships(d, nr_x, nr_p, exponent);
        ufcl_prototypes(x, u, w, nr_x, nc, nr_p, f, dist);
        new_value = cmeans_error_fn(u, d, w, nr_x, nr_p, f);

        if (abs(old_value - new_value) < (reltol * (old_value + reltol))):
            if verbose:
                print(f'Iteration {iter}, Error: {new_value}.')
                break;
        else:
            if verbose:
                ermin = cmeans_error_fn(u, d, w, nr_x, nr_p, f);
                print(f'Iteration {iter}, Error: {new_value}.')
            old_value = new_value;

        iter = iter + 1
    ermin = new_value


def print_d():
    print(d)

###################################################

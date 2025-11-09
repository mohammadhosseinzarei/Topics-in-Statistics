# mohammad hossein zarei

# تعریف ماتریس A
A <- matrix(c(13, -4, 2,
              -4, 13, -2,
              2, -2, 10), 
            nrow = 3, byrow = TRUE)


A

# ترانهاده
t(A)

# دترمینان
det(A)


# مقادیر ویژه
eigen(A)$values

# بردارهای ویژه
eigen(A)$vectors

# معکوس
print("Inverse of A:")
print(solve(A))

دش# ضرب A در خو
A %*% A

# ماتریس همانی
diag(3)

# رد ماتریس (Trace)
sum(diag(A)

B <- matrix(c(13, -4, 2,
              -4, 13, -2,
              2, -2, 10), nrow = 3, byrow = TRUE)

isSymmetric(B)


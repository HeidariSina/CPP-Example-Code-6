#include "hw1.h"

#include <iomanip>
#include <random>

using namespace algebra;
// func 1
Matrix algebra::zeros(size_t n, size_t m)
{
    std::vector<double> vect(m, 0.0);
    Matrix matrix(n, vect);
    return matrix;
}

// func 2
Matrix algebra::ones(size_t n, size_t m)
{
    std::vector<double> vect(m, 1.0);
    Matrix matrix(n, vect);
    return matrix;
}

// func 3
Matrix algebra::random(size_t n, size_t m, double min, double max)
{
    if (min > max)
    {
        throw std::logic_error("Caution: min cannot be greater than max");
    }
    double temp{0};
    Matrix matrix{};

    std::random_device rd;
    std::default_random_engine eng(rd());
    std::uniform_real_distribution<double> distr(min, max);

    std::vector<double> a{};

    for (size_t i{0}; i < n; i++)
    {
        for (size_t i{0}; i < m; i++)
        {
            temp = distr(eng);
            a.push_back(temp);
        }
        matrix.push_back(a);
        a.clear();
    }
    return matrix;
}

// func 4
void algebra::show(const Matrix &matrix)
{
    for (auto row : matrix)
    {
        for (auto col : row)
        {
            std::cout << std::setw(3) << std::setprecision(3) << col;
        }
        std::cout << std::endl;
    }
}
// func 5
Matrix algebra::multiply(const Matrix &matrix, double c)
{
    std::vector<double> temp{};
    Matrix multi{};

    for (auto row : matrix)
    {
        for (auto col : row)
        {
            temp.push_back(col * c);
        }
        multi.push_back(temp);
        temp.clear();
    }
    return multi;
}

// func 6
Matrix algebra::multiply(const Matrix &matrix1, const Matrix &matrix2)
{
    size_t height1{matrix1.size()};
    size_t height2{matrix2.size()};
    size_t length1{};
    size_t length2{};

    if (height1 > 0)
        length1 = matrix1[0].size();
    else
        length1 = 0;

    if (height2 > 0)
        length2 = matrix2[0].size();
    else
        length2 = 0;
    std::vector<double> temp{};
    Matrix multi{};
    double add{0};

    if (length1 != height2)
    {
        throw std::logic_error("Caution: matrices with wrong dimensions cannot be multiplied");
    }

    for (size_t i{0}; i < height1; i++)
    {
        for (size_t j{0}; j < length2; j++)
        {
            for (size_t k{0}; k < length1; k++)
            {
                add += matrix1[i][k] * matrix2[k][j];
            }
            temp.push_back(add);
            add = 0;
        }
        multi.push_back(temp);
        temp.clear();
    }
    return multi;
}

// func 7
Matrix algebra::sum(const Matrix &matrix, double c)
{
    std::vector<double> temp{};
    Matrix add{};

    for (auto row : matrix)
    {
        for (auto col : row)
        {
            temp.push_back(col + c);
        }
        add.push_back(temp);
        temp.clear();
    }
    return add;
}

// func 8
Matrix algebra::sum(const Matrix &matrix1, const Matrix &matrix2)
{
    size_t height1{matrix1.size()};
    size_t height2{matrix2.size()};
    size_t length1{};
    size_t length2{};

    if (height1 > 0)
        length1 = matrix1[0].size();
    else
        length1 = 0;

    if (height2 > 0)
        length2 = matrix2[0].size();
    else
        length2 = 0;

    std::vector<double> temp{};
    Matrix add{};

    if ((height1 != height2) || (length1 != length2))
    {
        throw std::logic_error("Caution: matrices with wrong dimensions cannot be summed");
    }

    for (size_t i{0}; i < matrix1.size(); i++)
    {
        for (size_t j{0}; j < matrix1[i].size(); j++)
        {
            temp.push_back(matrix1[i][j] + matrix2[i][j]);
        }
        add.push_back(temp);
        temp.clear();
    }
    return add;
}

// func 9
Matrix algebra::transpose(const Matrix &matrix)
{
    size_t height{matrix.size()};
    size_t length{};
    if (height > 0)
        length = matrix[0].size();
    else
        length = 0;
    Matrix tran{};
    std::vector<double> temp{};

    for (size_t i{0}; i < length; i++)
    {
        for (size_t j{0}; j < height; j++)
        {
            temp.push_back(matrix[j][i]);
        }
        tran.push_back(temp);
        temp.clear();
    }
    return tran;
}

// func 10
Matrix algebra::minor(const Matrix &matrix, size_t n, size_t m)
{
    size_t height{matrix.size()};
    size_t length{};
    if (height > 0)
        length = matrix[0].size();
    else
        length = 0;
    Matrix tran{};
    std::vector<double> temp{};

    for (size_t i{0}; i < height; i++)
    {
        if (i == n)
            continue;
        for (size_t j{0}; j < length; j++)
        {
            if (j == m)
                continue;
            temp.push_back(matrix[i][j]);
        }
        tran.push_back(temp);
        temp.clear();
    }
    return tran;
}

// func 11
double algebra::determinant(const Matrix &matrix)
{
    double det{0}, temp{0}, x{0};
    Matrix minor{};
    size_t height{matrix.size()};
    size_t length{};

    if (height > 0)
        length = matrix[0].size();
    else
        length = 0;
    if (length != height)
        throw std::logic_error("Caution: non-square matrices have no determinant");
    if (length == 0)
        return 1.0;

    if (length == 1)
        return matrix[0][0];
    else if (length == 2)
        return (matrix[0][0] * matrix[1][1]) - (matrix[0][1] * matrix[1][0]);
    else
    {
        for (size_t i{0}; i < height; i++)
        {
            temp = algebra::determinant(algebra::minor(matrix, i, 0));
            if (i % 2 == 0)
                det += matrix[i][0] * temp;
            else
                det -= matrix[i][0] * temp;
        }
        return det;
    }
}

// func 12
Matrix algebra::inverse(const Matrix &matrix)
{
    size_t height{matrix.size()};
    size_t length{};
    Matrix inv{};
    std::vector<double> temp{};

    if (height > 0)
        length = matrix[0].size();
    else
        length = 0;

    if (length != height)
        throw std::logic_error("Caution: non-square matrices have no inverse");

    double det{algebra::determinant(matrix)}, d{0};
    if (det == 0.0)
        throw std::logic_error("Caution: singular matrices have no inverse");

    for (size_t i{0}; i < height; i++)
    {
        for (size_t j{0}; j < length; j++)
        {
            d = algebra::determinant(algebra::minor(matrix, i, j));
            if ((i + j) % 2 == 0)
                temp.push_back(d);
            else
                temp.push_back(-d);
        }
        inv.push_back(temp);
        temp.clear();
    }
    inv = algebra::transpose(inv);
    return algebra::multiply(inv, 1 / det);
}

// func 13
Matrix algebra::concatenate(const Matrix &matrix1, const Matrix &matrix2, int axis = 0)
{
    Matrix con{};
    std::vector<double> temp{};
    size_t height1{matrix1.size()};
    size_t height2{matrix2.size()};
    size_t length1{};
    size_t length2{};

    if (height1 > 0)
        length1 = matrix1[0].size();
    else
        length1 = 0;

    if (height2 > 0)
        length2 = matrix2[0].size();
    else
        length2 = 0;

    if (axis == 0)
    {
        if (length1 != length2)
            throw std::logic_error("Caution: matrices with wrong dimensions cannot be concatenated");
        for (auto row : matrix1)
        {
            for (auto col : row)
            {
                temp.push_back(col);
            }
            con.push_back(temp);
            temp.clear();
        }
        for (auto row : matrix2)
        {
            for (auto col : row)
            {
                temp.push_back(col);
            }
            con.push_back(temp);
            temp.clear();
        }
    }
    else
    {
        if (height1 != height2)
            throw std::logic_error("Caution: matrices with wrong dimensions cannot be concatenated");
        for (size_t i{0}; i < height1; i++)
        {
            for (size_t j{0}; j < length1; j++)
            {
                temp.push_back(matrix1[i][j]);
            }
            for (size_t j{0}; j < length2; j++)
            {
                temp.push_back(matrix2[i][j]);
            }
            con.push_back(temp);
            temp.clear();
        }
    }
    return con;
}

// func 14
Matrix algebra::ero_swap(const Matrix &matrix, size_t r1, size_t r2)
{
    Matrix ero{matrix};
    double temp{0};
    size_t height{matrix.size()};
    size_t length{};
    if (height > 0)
        length = matrix[0].size();
    else
        length = 0;

    if (r1 >= height || r2 >= height)
        throw std::logic_error("Caution: r1 or r2 inputs are out of range");
    for (size_t i{0}; i < length; i++)
    {
        temp = ero[r1][i];
        ero[r1][i] = ero[r2][i];
        ero[r2][i] = temp;
    }
    return ero;
}

// func 15
Matrix algebra::ero_multiply(const Matrix &matrix, size_t r, double c)
{
    Matrix ero{matrix};
    size_t height{matrix.size()};
    size_t length{};
    if (height > 0)
        length = matrix[0].size();
    else
        length = 0;

    if (r >= height)
        throw std::logic_error("Caution: r inputs are out of range");

    for (size_t i{0}; i < length; i++)
    {
        ero[r][i] *= c;
    }
    return ero;
}

// func 16
Matrix algebra::ero_sum(const Matrix &matrix, size_t r1, double c, size_t r2)
{
    Matrix ero{matrix};
    size_t height{matrix.size()};
    size_t length{};
    if (height > 0)
        length = matrix[0].size();
    else
        length = 0;

    if (r1 >= height || r2 >= height)
        throw std::logic_error("Caution: r1 or r2 inputs are out of range");

    for (size_t i{0}; i < length; i++)
    {
        ero[r2][i] += (ero[r1][i] * c);
    }
    return ero;
}

// func 17
Matrix algebra::upper_triangular(const Matrix &matrix)
{
    double count{0};
    size_t temp{0};
    Matrix ero{matrix};
    size_t height{matrix.size()};
    size_t length{};
    if (height > 0)
        length = matrix[0].size();
    else
        return ero;

    if (length != height)
        throw std::logic_error("non-square matrices have no upper triangular form");
    for (size_t i{0}; i < height - 1; i++)
    {
        temp = i + 1;
        while (ero[i][i] == 0)
        {
            if (temp == height)
                throw std::logic_error("this matrix dont have a triangular form");
            ero = algebra::ero_swap(ero, i, temp);
            temp++;
        }
        for (size_t j{i + 1}; j < height; j++)
        {
            ero = algebra::ero_sum(ero, i, -(ero[j][i] / ero[i][i]), j);
        }
    }
    return ero;
}
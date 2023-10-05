#pragma once

#include <GL/glew.h>
#include <vector>
struct point {
	GLfloat x;
	GLfloat y;
};


point pascalSnail(float theta, float a, float l);

std::vector<point> plotPascalSnail(int nPoints);
std::vector<point> plotGrid(int rows, int cols);

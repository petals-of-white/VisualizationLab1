#include "Plots.h"
#include "GLM/glm.hpp"
#include <array>
#include <GLM/ext/scalar_constants.hpp>
point pascalSnail(float theta, float a, float l) {
	auto r = l - a * sin(theta);
	auto x = (l + a * cos(theta)) * cos(theta);
	auto y = (l + a * cos(theta)) * sin(theta);
	return { x, y };
}

std::vector<point> plotPascalSnail(int nPoints) {

	std::vector<point> graph(2000);
	for (auto i = 0; i < graph.size(); i++)
	{
		auto theta = i / static_cast<float>(graph.size()) * 2.0f * glm::pi<float>();
		auto p = pascalSnail(theta, 0.3f, 0.5f);
		graph[i] = p;
	}
	return graph;
}

std::vector<point> plotGrid(int rows, int cols)
{
	std::vector<point> rowV((rows - 1) * 2);
	std::vector<point> colV((cols - 1) * 2);

	auto rowstep = 2.0f / rows;
	auto columnstep = 2.0f / cols;

	// rows

	for (auto i = 0; i < (rows - 1) * 2; i += 2)
	{
		auto y = -1.0f + (i + 2) / 2 * rowstep;
		rowV[i].x = -1.0f;
		rowV[i].y = y;
		rowV[i + 1].x = 1.0f;
		rowV[i + 1].y = y;
	}

	// cols
	for (auto i = 0; i < (cols - 1) * 2; i += 2)
	{
		auto x = -1.0f + (i + 2) / 2 * columnstep;
		colV[i].y = -1.0f;
		colV[i].x = x;

		colV[i + 1].y = 1.0f;
		colV[i + 1].x = x;
	}

	std::vector<point> grid;
	grid.insert(grid.end(), rowV.begin(), rowV.end());
	grid.insert(grid.end(), colV.begin(), colV.end());

	return grid;
}
#include <GL/glew.h>
#include <GLFW/glfw3.h>
#include <iostream>

#include <fstream>
#include <string>
#include <sstream>
#include "Shaders.h"
#include "VAO.h"
#include "VBO.h"
#include "EBO.h"
#include "Macros.h"
#include "GLM/glm.hpp"
#include "GLM/gtc/matrix_transform.hpp"
#include "GLM/gtc/type_ptr.hpp"
#include <array>
#include <vector>
#include "Plots.h"

GLfloat vertices[] =
{//			COORDINATES										/ COLORS					//
	-0.5f, -0.5f * float(sqrt(3)) / 3, 		0.0f,	0.8f, 0.3f, 0.02f, // Lower left corner
	0.5f, -0.5f * float(sqrt(3)) / 3, 			0.0f,	0.8f, 0.3f, 0.02f,			// Lower right corner
	0.0f, 0.5f * float(sqrt(3)) * 2 / 3, 		0.0f,	1.0f, 0.6f, 0.32f,		// Upper corner
	-0.5f / 2, 0.5f * float(sqrt(3)) / 6, 		0.0f,	0.9f, 0.45f, 0.17f,// Inner left
	0.5f / 2, 0.5f * float(sqrt(3)) / 6, 		0.0f,	0.9f, 0.45f, 0.17f, // Inner right
	0.0f, -0.5f * float(sqrt(3)) / 3, 			0.0f,	0.9f, 0.3f, 0.02f// Inner down
};

// Indices for vertices order
GLuint indices[] =
{
	0, 3, 5, // Lower left triangle
	3, 2, 4, // Lower right triangle
	5, 4, 1 // Upper triangle
};

int width = 1200, height = 1200;
GLfloat rotAngle = 50, scaleX = 1.0f, offsetX = 0.0f;


int main(void)
{
	auto graph = plotPascalSnail(2000);
	auto grid = plotGrid(10, 10);

	std::vector<point> allPoints;

	allPoints.insert(allPoints.begin(), grid.begin(), grid.end());
	allPoints.insert(allPoints.end(), graph.begin(), graph.end());

	GLFWwindow* window;

	/* Initialize the library */
	if (!glfwInit())
		return -1;


	/* Create a windowed mode window and its OpenGL context */
	window = glfwCreateWindow(width, height, "Равлик паскаля", NULL, NULL);
	if (!window)
	{
		glfwTerminate();
		return -1;
	}

	/* Make the window's context current */
	glfwMakeContextCurrent(window);
	glewInit();

	// Specify the viewport of OpenGL in the Windows
	// in this case the viewport goes from x = 0, y = 0, to x = 800, y = 800
	GLCall(glViewport(0, 0, width, height));

	std::cout << glGetString(GL_VERSION) << std::endl;

	Shader graphShader("res/shaders/Graph.vert", "res/shaders/Graph.frag");
	Shader gridShader("res/shaders/Grid.vert", "res/shaders/Grid.frag");
	glm::vec4 vec(1.0f, 1.0f, 0.0f, 1.0f);
	glm::mat4 trans = glm::mat4(1.0f);
	//trans = glm::translate(trans, glm::vec3(offsetX, 0.0f, 0.0f));
	//trans = glm::rotate(trans, glm::radians(90.0f), glm::vec3(0.0f, 0.0f, 1.0f));
	//trans = glm::scale(trans, glm::vec3(scaleX, 1.0f, 1.0f));
	vec = trans * vec;
	std::cout << vec.x << vec.y << vec.z << std::endl;

	//VAO VAO1;
	VAO gridVAO;
	VAO graphVAO;
	//VAO1.Bind();

	//VBO VBO1(vertices, sizeof(vertices));

	gridVAO.Bind();
	VBO gridVBO((GLfloat*)(grid.data()), grid.size() * sizeof(point));
	gridVAO.LinkAttrib(gridVBO, 0, 2, GL_FLOAT, sizeof(point), nullptr);

	graphVAO.Bind();
	VBO graphVBO((GLfloat*)(graph.data()), graph.size() * sizeof(point));
	graphVAO.LinkAttrib(graphVBO, 0, 2, GL_FLOAT, sizeof(point), nullptr);

	
	//VBO VBO1((GLfloat*)(allPoints.data()), allPoints.size()*sizeof(point));
	
	//EBO EBO1(indices, sizeof(indices));

	//VAO1.LinkAttrib(VBO1, 0, 2, GL_FLOAT, sizeof(point), nullptr);
	//VAO1.LinkAttrib(graphVBO, 0, 2, GL_FLOAT, sizeof(point), nullptr);
	//VAO1.Unbind();

	
	/*gridVAO.Unbind();*/
	//VAO1.LinkAttrib(VBO1, 0, 3, GL_FLOAT, 6 * sizeof(float), nullptr);
	//VAO1.LinkAttrib(VBO1, 1, 3, GL_FLOAT, 6 * sizeof(float), (void*)(3 * sizeof(float)));

	gridVAO.Unbind();
	graphVAO.Unbind();
	//VAO1.Unbind();
	//VBO1.Unbind();
	
	//EBO1.Unbind();

	graphShader.Activate();

	//GLCall(auto transformLoc = glGetUniformLocation(shaderProgram.ID, "u_transform"));
	GLCall(auto offsetXLoc = glGetUniformLocation(graphShader.ID, "offset_x"));
	GLCall(auto scaleXLoc = glGetUniformLocation(graphShader.ID, "scale_x"));
	GLCall(glUniform1f(offsetXLoc, offsetX));
	GLCall(glUniform1f(scaleXLoc, scaleX));
	//GLCall(glUniformMatrix4fv(transformLoc, 1, GL_FALSE, glm::value_ptr(trans)));

	GLCall(glClearColor(0.0f, 0.0f, 0.0f, 1.0f));

	/* Loop until the user closes the window */
	while (!glfwWindowShouldClose(window))
	{
		/* Render here */
		GLCall(glClear(GL_COLOR_BUFFER_BIT));
		
		//VAO1.Bind();
		gridVAO.Bind();
		gridShader.Activate();
		GLCall(glDrawArrays(GL_LINES, 0, grid.size()));


		graphVAO.Bind();
		graphShader.Activate();
		GLCall(glDrawArrays(GL_LINES, 0, graph.size()));

		/* Swap front and back buffers */
		glfwSwapBuffers(window);

		/* Poll for and process events */
		glfwPollEvents();
	}

	// DELETE EVERy object we've created
	//VAO1.Delete();
	//VBO1.Delete();
	//EBO1.Delete();
	gridVAO.Delete();
	graphVAO.Delete();
	gridVBO.Delete();
	graphVBO.Delete();
	graphShader.Delete();
	gridShader.Delete();
	glfwTerminate();
	return 0;
}
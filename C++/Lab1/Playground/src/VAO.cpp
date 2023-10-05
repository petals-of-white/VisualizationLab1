#include "VAO.h"
#include "Macros.h"

VAO::VAO()
{
	GLCall(glGenVertexArrays(1, &ID));

}

void VAO::LinkAttrib(VBO VBO, GLuint layout, GLuint numComponents, GLenum type, GLsizeiptr stride, void* offset) 
{
	VBO.Bind();
	//enable the vertex attribute so that OPENGL know to use it
	GLCall(glEnableVertexAttribArray(layout));
	GLCall(glVertexAttribPointer(layout, numComponents, type, GL_FALSE, stride, offset));


	VBO.Unbind();
}

void VAO::Bind()
{
	GLCall(glBindVertexArray(ID));
}

void VAO::Unbind()
{
	GLCall(glBindVertexArray(0));
}

void VAO::Delete()
{
	GLCall(glDeleteVertexArrays(1, &ID));
}

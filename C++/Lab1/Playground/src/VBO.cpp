#include "VBO.h"
#include "Macros.h"
VBO::VBO(GLfloat* vertices, GLsizeiptr size)
{
	GLCall(glGenBuffers(1, &ID));
	Bind();
	GLCall(glBufferData(GL_ARRAY_BUFFER, size, vertices, GL_STATIC_DRAW));
}

void VBO::Bind()
{
	GLCall(glBindBuffer(GL_ARRAY_BUFFER, ID));
}

void VBO::Unbind()
{
	GLCall(glBindBuffer(GL_ARRAY_BUFFER, 0));
}

void VBO::Delete()
{
	GLCall(glDeleteBuffers(1, &ID));
}
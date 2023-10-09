#version 410 core
layout (location = 0) in vec2 aPos;
out vec4 color;
void main()
{
	gl_Position = vec4(aPos, 0.0f, 1.0f);
}
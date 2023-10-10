#version 410 core
layout (location = 0) in vec2 aPos;

out vec4 color;
uniform mat4 u_transform;
void main()
{
	gl_Position = u_transform * vec4(aPos, 0.0f, 1.0f);
	color = vec4(aPos.xy / 2.0 + 0.5, 1, 1);
}
#include "Shaders.h"

#include <fstream>
#include <sstream>
#include <iostream>
#include "Macros.h"

std::string get_file_contents(const char* filename)
{
	std::ifstream in(filename, std::ios::binary);
	
	std::stringstream ss;

	ss << in.rdbuf();
	return ss.str(); 
}

Shader::Shader(const char* vertexFile, const char* fragmentFile) {
	std::string vertexCode = get_file_contents(vertexFile);
	std::string fragmentCode = get_file_contents(fragmentFile);

	// Convert the shader source strings into character arrays
	const char* vertexSource = vertexCode.c_str();
	const char* fragmentSource = fragmentCode.c_str();
	GLCall(GLuint vertexShader = glCreateShader(GL_VERTEX_SHADER));
	GLCall(glShaderSource(vertexShader, 1, &vertexSource, NULL));

	GLCall(glCompileShader(vertexShader));

	GLCall(GLuint fragmentShader = glCreateShader(GL_FRAGMENT_SHADER));

	GLCall(glShaderSource(fragmentShader, 1, &fragmentSource, NULL));

	GLCall(glCompileShader(fragmentShader));

	GLCall(ID = glCreateProgram());

	GLCall(glAttachShader(ID, vertexShader));
	GLCall(glAttachShader(ID, fragmentShader));

	GLCall(glLinkProgram(ID));
	GLCall(glDeleteShader(vertexShader));
	GLCall(glDeleteShader(fragmentShader));
	
}

void Shader::Activate() {
	GLCall(glUseProgram(ID));
}
void Shader::Delete() {
	GLCall(glDeleteProgram(ID));
}

ShaderProgramSource ParseShader(const std::string& vertexPath, const std::string& fragmentPath)
{
	std::ifstream v(vertexPath), f(fragmentPath);

	enum ShaderType
	{
		NONE = -1, VERTEX = 0, FRAGMENT = 1
	};

	std::string line;

	std::stringstream ss[2];
	
	ss[0] << v.rdbuf();
	ss[1] << f.rdbuf();
	return { ss[0].str(), ss[1].str() };
}

unsigned int CompileShader(unsigned int type, const std::string& source)
{
	unsigned int id = glCreateShader(type);
	const char* src = source.c_str();
	GLCall(glShaderSource(id, 1, &src, nullptr));
	GLCall(glCompileShader(id));
	int result;
	GLCall(glGetShaderiv(id, GL_COMPILE_STATUS, &result));

	if (result == GL_FALSE) {
		int length;
		glGetShaderiv(id, GL_INFO_LOG_LENGTH, &length);
		char* message = (char*)alloca(length * sizeof(char));

		glGetShaderInfoLog(id, length, &length, message);
		std::cout << "Failed to compile shader! " <<
			(type == GL_VERTEX_SHADER ? "vertex" : "fragment") << " shader!" << std::endl;
		std::cout << message << std::endl;
		glDeleteShader(id);
		return 0;
	}

	// TODO: Error handling

	return id;
}

unsigned int CreateShader(const std::string& vertexShader, const std::string& fragmentShader)
{
	unsigned int program = glCreateProgram();
	unsigned int vs = CompileShader(GL_VERTEX_SHADER, vertexShader);
	unsigned int fs = CompileShader(GL_FRAGMENT_SHADER, fragmentShader);

	glAttachShader(program, vs);
	glAttachShader(program, fs);
	glLinkProgram(program);
	glValidateProgram(program);

	glDeleteShader(vs);
	glDeleteShader(fs);

	return program;
}
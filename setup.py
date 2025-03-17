from setuptools import setup, find_packages

setup(
    name="cobtran",
    version="0.1.0",
    packages=find_packages(),
    include_package_data=True,
    install_requires=[
        "lark>=1.1.5",
        "click>=8.1.3",
        "jinja2>=3.1.2",
    ],
    entry_points={
        "console_scripts": [
            "cobtran=cobtran.cli:cli",
        ],
    },
    package_data={
        "cobtran.parser": ["*.lark"],
    },
    author="Your Name",
    author_email="your.email@example.com",
    description="A tool for migrating COBOL code to modern programming languages",
    keywords="cobol, migration, transpiler, python, java",
    python_requires=">=3.8",
    classifiers=[
        "Development Status :: 3 - Alpha",
        "Intended Audience :: Developers",
        "Programming Language :: Python :: 3",
        "Programming Language :: Python :: 3.8",
        "Programming Language :: Python :: 3.9",
        "Programming Language :: Python :: 3.10",
        "Topic :: Software Development :: Code Generators",
        "Topic :: Software Development :: Compilers",
    ],
)

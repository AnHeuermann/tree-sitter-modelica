model HelloWorld "Simple Hello World Modelica example"
  Real x(start=1, fixed=true);
equation
  der(x) = -0.5 * x;
end HelloWorld;

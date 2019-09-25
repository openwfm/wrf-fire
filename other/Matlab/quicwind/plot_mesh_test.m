function plot_mesh_test
X = uniform_mesh([10,10,10],[1,1,2]);
X1 = add_terrain_to_mesh(X,'hill','shift',0.2);
figure(1)
plot_mesh(X1)
X2 = add_terrain_to_mesh(X,'hill','squash',0.2);
figure(2)
plot_mesh(X2)
XR = regular_mesh([10,10,10],[1,1,2],1.2);
X3 = add_terrain_to_mesh(XR,'hill','shift',0.1);
figure(3)
plot_mesh(X3)
X4 = add_terrain_to_mesh(XR,'hill','squash',0.1);
figure(4)
plot_mesh(X4)
end
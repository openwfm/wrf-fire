X = uniform_mesh([10,10,10],[1,1,2]);
X1 = add_terrain_to_mesh(X,'hill','shift')
figure(1)
plot_mesh(X1)
X2 = add_terrain_to_mesh(X,'hill','squash')
figure(2)
plot_mesh(X2)

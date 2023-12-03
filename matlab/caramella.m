function poligono = caramella()

warning('off', 'MATLAB:polyshape:repairedBySimplify');
coord_poligono = [-0.4 0.2; 0 -0.2; 0.4 0.2; 0.4 -0.2; 0 0.2; -0.4 -0.2];
poligono = polyshape(coord_poligono);
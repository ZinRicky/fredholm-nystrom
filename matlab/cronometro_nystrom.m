function [t_cubatura, t_compressione, t_sys, t_sys_c] = ...
    cronometro_nystrom(ade, poligono, K, y, lambda)
tic;
xyw = polygauss_2018(ade, poligono);
t_cubatura = toc;

tic;
[xyc, wc] = comprexcub(ade, xyw(:, 1:2), xyw(:, 3), 1);
t_compressione = toc;

tic;
[~, ~, ~] = sistema_lineare_nystrom(...
    xyw(:, 1:2), xyw(:, 3), K, y, lambda);
t_sys = toc;

tic;
[~, ~, ~] = sistema_lineare_nystrom(...
    xyc, wc, K, y, lambda);
t_sys_c = toc;
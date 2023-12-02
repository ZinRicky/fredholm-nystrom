function [err, err_c, numero_nodi, numero_nodi_c, condiz, condiz_c, ...
    x_controllo, x_controllo_c] = ...
    errori_nystrom(ade, poligono, K, y, lambda, x_vera)

xyw = polygauss_2018(ade, poligono);
[xyc, wc] = comprexcub(ade, xyw(:, 1:2), xyw(:, 3), 1);

[soluzione, matrice, numero_nodi] = sistema_lineare_nystrom(...
    xyw(:, 1:2), xyw(:, 3), K, y, lambda);
[soluzione_c, matrice_c, numero_nodi_c] = sistema_lineare_nystrom(...
    xyc, wc, K, y, lambda);

condiz = cond(matrice);
condiz_c = cond(matrice_c);

x_controllo = x_vera(xyw(:,1), xyw(:,2));
x_controllo_c = x_vera(xyc(:,1), xyc(:,2));
err = abs(x_controllo - soluzione);
err_c = abs(x_controllo_c - soluzione_c);
function demo_caramella(tipo, n_lambda, max_ade)

% Demo errori e condizionamento sul poligono con autointersezioni $D_2$

if nargin < 1
    tipo = 'exp';
end
if nargin < 2
    n_lambda = 1;
end
if nargin < 3
    max_ade = 20;
end

poligono = caramella();

switch tipo
    case 'exp'
        K = @(t1, t2, s1, s2) (s1.^2 - s2.^2) .* exp(s1 + s2 + t1 + t2);
        x_vera = @(t1, t2) (t1 + t2).^2;
        fprintf('Nucleo esponenziale.\n\n');
    case 'exp2'
        K = @(t1, t2, s1, s2) (s1.^2 - s2.^2) .* exp(s1 + s2 + t1 + t2);
        x_vera = @(t1, t2) abs(t1 + t2);
        fprintf('Nucleo esponenziale, soluzione non liscia.\n\n');
    case 'sin'
        K = @(t1, t2, s1, s2) s1 .* s2 .* sin(t1 + t2);
        x_vera = @(t1, t2) (t1 + t2).^2;
        fprintf('Nucleo sinusoidale.\n\n');
    case 'sqrt'
        K = @(t1, t2, s1, s2) sqrt(abs(t1 .* t2)) + sqrt(abs(s1 .* s2));
        x_vera = @(t1, t2) abs(t1 + t2);
        fprintf('Nucleo con radici quadrate, soluzione non liscia.\n\n');
    otherwise
        error('Esempio non supportato in questa demo.');
end

lambda_demo = [0.001 0.01 0.1 1 10];
for lambda = lambda_demo((6-n_lambda):5)
    switch tipo
        case 'exp'
            y = @(t1, t2) (1/6250)*exp((-3/5)+t1+t2)* ...
                (242565-162599*exp(2/5)-207241*exp(4/5)+138915*exp(6/5)) ...
                +lambda * x_vera(t1, t2);
        case 'exp2'
            y = @(t1, t2) -((1/625).*exp((-3/5)+t1+t2)* ...
                (-4440+2977*exp(2/5)-4452*exp(4/5)+2985*exp(6/5))) ...
                +lambda * x_vera(t1, t2);
        case 'sin'
            y = @(t1, t2) -4/28125 * sin(t1 + t2) + ...
                lambda .* x_vera(t1, t2);
        case 'sqrt'
            y = @(t1, t2) ...
                (-1/30000).*(4+39.*sqrt(2)+6.*sqrt(3)+(-5).*pi+ ...
                1040.*sqrt(abs(t1.*t2))-3*acsch(sqrt(2))+3.*asinh(1)) ...
                +lambda * x_vera(t1, t2);
    end
    fprintf("_________________\nlambda = %g\n_________________\n", lambda);
    fprintf("%3s %6s %16s %16s %16s %6s %16s %16s %16s\n", ...
    "ADE", "nodi", "err. 2", "err. max.", "condizion.", ...
    "nodi-C", "err. 2-C", "err. max.-C", "condizion.-C");
    for ade = 1:max_ade
        [err, err_c, numero_nodi, numero_nodi_c, condiz, condiz_c, ...
        x_controllo, x_controllo_c] = ...
        errori_nystrom(ade, poligono, K, y, lambda, x_vera);

        err_2 = norm(err) / norm(x_controllo);
        err_2_c = norm(err_c) / norm(x_controllo_c);
        err_inf = norm(err, Inf) / norm(x_controllo, Inf);
        err_inf_c = norm(err_c, Inf) / norm(x_controllo_c, Inf);
        
        fprintf("%3d %6d %16.10e %16.10e %16.10e %6d %16.10e %16.10e %16.10e\n", ...
        ade, numero_nodi, err_2, err_inf, condiz, numero_nodi_c, ...
        err_2_c, err_inf_c, condiz_c);
    end
end
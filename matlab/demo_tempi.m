function demo_tempi(max_ade)

if nargin < 1
    max_ade = 20;
end

fprintf('Prova tempo per equazioni con nucleo esponenziale e soluzione liscia.\n');
fprintf('Sono disponibili i seguenti domini:\n• %s\n• %s\n• %s\n• %s\n\n', ...
    'esagono', 'caramella', 'caramellone', 'concerchio');
dominio = input('Su che dominio si vuole effettuare la prova tempo? (Default: caramellone)\n> ', 's');
switch dominio
    case 'esagono'
        poligono = esagono();
        K = @(t1, t2, s1, s2) exp(s1 .* s2 + t1 .* t2);
        x_vera = @(t1, t2) t1 + t2;
        y = @(t1, t2) -0.03022286918687376*exp(t1.*t2)+x_vera(t1, t2);
    case 'caramella'
        poligono = caramella();
        K = @(t1, t2, s1, s2) (s1.^2 - s2.^2) .* exp(s1 + s2 + t1 + t2);
        x_vera = @(t1, t2) (t1 + t2).^2;
        y = @(t1, t2) ...
            (1/6250).*exp((-3/5)+t1+t2).*(242565+(-162599).*exp(2/5) ...
            +(-207241).*exp(4/5)+138915.*exp(6/5)) ...
            +x_vera(t1, t2);
    case {'caramellone', ''}
        poligono = caramellone();
        K = @(t1, t2, s1, s2) (s1.^2 - s2.^2) .* exp(s1 + s2 + t1 + t2);
        x_vera = @(t1, t2) (t1 + t2).^2;
        y = @(t1, t2) -6.36002E-4 * exp(t1 + t2) + x_vera(t1, t2);
    case 'concerchio'
        poligono = concerchio();
        K = @(t1, t2, s1, s2) (s1.^2 - s2.^2) .* exp(s1 + s2 + t1 + t2);
        x_vera = @(t1, t2) (t1 + t2).^2;
        y = @(t1, t2) 0.08131737588823632 * exp(t1 + t2) +x_vera(t1, t2);
    otherwise
        error('Non supportato (i nomi sono sempre scritti senza maiuscole)');
end

fprintf("%3s %16s %16s %16s %16s %16s %16s\n", ...
    "ADE", "t_cubatura", "t_compressione", "t_sys", "t_sys_c", ...
    "tot_senza", "tot_catch");
vinto = 0;

for ade = 1:max_ade
    [t_cubatura, t_compressione, t_sys, t_sys_c] = cronometro_nystrom(ade, ...
    poligono, K, y, 1);

    fprintf(...
        "%3d %16.10e %16.10e %16.10e %16.10e %16.10e %16.10e\n", ...
        ade, t_cubatura, t_compressione, t_sys, t_sys_c, ...
        t_cubatura + t_sys, t_cubatura + t_compressione + t_sys_c);
    if t_cubatura + t_sys > t_cubatura + t_compressione + t_sys_c
        vinto = vinto + 1;
    end
end

fprintf('\nComprimere ha dato un risultato in tempo minore in %d casi (%.2f%%).\n\n', ...
    vinto, vinto / ade * 100);
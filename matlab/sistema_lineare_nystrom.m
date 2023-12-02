function [soluzione, matrice, numero_nodi] = sistema_lineare_nystrom(...
    XY, W, K, y, lambda)

numero_nodi = length(W);
matrice = zeros(numero_nodi, numero_nodi);
for riga = 1:numero_nodi
    for colonna = 1:numero_nodi
        matrice(riga,colonna) = lambda * (riga == colonna) - W(colonna) ...
            * K(XY(riga,1), XY(riga,2), XY(colonna,1), XY(colonna,2));
    end
end

termine_noto = y(XY(:,1), XY(:,2));
soluzione = matrice \ termine_noto;
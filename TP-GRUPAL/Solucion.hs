-- Completar con los datos del grupo
--
-- Nombre de Grupo: xx
-- Integrante 1: Aaron Cuellar, aaroncuellar2003@gmail.com, 810/23
-- Integrante 2: Nombre Apellido, email, LU
-- Integrante 3: Nombre Apellido, email, LU
-- Integrante 4: Nombre Apellido, email, LU

module Solucion where

type Usuario = (Integer, String) -- (id, nombre)
type Relacion = (Usuario, Usuario) -- usuarios que se relacionan
type Publicacion = (Usuario, String, [Usuario]) -- (usuario que publica, texto publicacion, likes)
type RedSocial = ([Usuario], [Relacion], [Publicacion])

-- Funciones basicas

usuarios :: RedSocial -> [Usuario]
usuarios (us, _, _) = us

relaciones :: RedSocial -> [Relacion]
relaciones (_, rs, _) = rs

publicaciones :: RedSocial -> [Publicacion]
publicaciones (_, _, ps) = ps

idDeUsuario :: Usuario -> Integer
idDeUsuario (id, _) = id 

nombreDeUsuario :: Usuario -> String
nombreDeUsuario (_, nombre) = nombre 

usuarioDePublicacion :: Publicacion -> Usuario
usuarioDePublicacion (u, _, _) = u

likesDePublicacion :: Publicacion -> [Usuario]
likesDePublicacion (_, _, us) = us



{- EJERCICIOS -}

--EJERCICIO 1
nombresDeUsuarios :: RedSocial -> [String]
nombresDeUsuarios ([], _, _) = []
nombresDeUsuarios ((u:us), rels, pubs) = nombreDeUsuario u : nombresDeUsuarios (us, rels, pubs)


--EJERCICIO 2
amigosDe :: RedSocial -> Usuario -> [Usuario]
amigosDe (_, [], _) _ = []
amigosDe (us, (u1, u2):rels, pubs) u | u == u2 = u1 : amigosDe (us, rels, pubs) u
                                     | u == u1 = u2 : amigosDe (us, rels, pubs) u
                                     | otherwise = amigosDe (us, rels, pubs) u


--EJERCICIO 3 
cantidadDeAmigos :: RedSocial -> Usuario -> Integer
cantidadDeAmigos red u = longitud (amigosDe red u)


--EJERCICIO 4
usuarioConMasAmigos :: RedSocial -> Usuario
usuarioConMasAmigos ([u], _, _) = u
usuarioConMasAmigos (u:us, rs, ps) | cantidadDeAmigos (u:us, rs, ps) u >= cantidadDeAmigos (us, rs, ps) (usuarioConMasAmigos (us, rs, ps)) = u
                                   | otherwise = usuarioConMasAmigos (us, rs, ps)


--EJERCICIO 5
estaRobertoCarlos :: RedSocial -> Bool
estaRobertoCarlos ([], _, _) = False
estaRobertoCarlos ((u:us), rels, pubs) | (nombreDeUsuario u == "Roberto Carlos") && (((cantidadDeAmigos ((u:us), rels, pubs)) u) > 1000000) = True
                                       | otherwise = estaRobertoCarlos (us, rels, pubs) 

--EJERCICIO 6
publicacionesDe :: RedSocial -> Usuario -> [Publicacion]
publicacionesDe (_, _, []) _ = []
publicacionesDe (us, rs, p:ps) u | usuarioDePublicacion p == u = p : publicacionesDe (us, rs, ps) u
                                 | otherwise = publicacionesDe (us, rs, ps) u



--EJERCICIO 7
publicacionesQueLeGustanA :: RedSocial -> Usuario -> [Publicacion]
publicacionesQueLeGustanA (_, _, []) _ = []
publicacionesQueLeGustanA (us, rs, p:ps) u | pertenece u (likesDePublicacion p) = p : publicacionesQueLeGustanA (us, rs, ps) u
                                           | otherwise = publicacionesQueLeGustanA (us, rs, ps) u



--EJERCICIO 8
lesGustanLasMismasPublicaciones :: RedSocial -> Usuario -> Usuario -> Bool
lesGustanLasMismasPublicaciones red u1 u2 = mismosElementos (publicacionesQueLeGustanA red u1) (publicacionesQueLeGustanA red u2)



--EJERCICIO 9
tieneUnSeguidorFiel :: RedSocial -> Usuario -> Bool
tieneUnSeguidorFiel ([], _, _) _ = False
tieneUnSeguidorFiel (u2:us, rs, ps) u1 | publicacionesDe (u2:us, rs, ps) u1 == [] = False --Esta línea verifica que el Usuario parámetro tenga al menos una publicación.
                                       | u1 /= u2 && perteneceLista (publicacionesDe (u2:us, rs, ps) u1) (publicacionesQueLeGustanA (u2:us, rs, ps) u2) = True
                                       | otherwise = tieneUnSeguidorFiel (us, rs, ps) u1

--aux
perteneceLista :: (Eq t) => [t] -> [t] -> Bool
perteneceLista [] _ = True
perteneceLista (x:xs) l2 | pertenece x l2 = perteneceLista xs l2
                         | otherwise = False


--EJERCICIO 10
existeSecuenciaDeAmigos :: RedSocial -> Usuario -> Usuario -> Bool
existeSecuenciaDeAmigos red u1 u2 = pertenece u2 (secuenciaDeAmigos red [u1] [])

--aux
secuenciaDeAmigos :: RedSocial -> [Usuario] -> [Usuario] -> [Usuario]
secuenciaDeAmigos _ [] cadena = cadena
secuenciaDeAmigos red (u:us) cadena | pertenece u cadena = secuenciaDeAmigos red us cadena
                                    | otherwise = secuenciaDeAmigos red (us ++ (amigosDe red u)) (u:cadena) 



{- PREDICADOS -}

--pred pertenece
pertenece :: (Eq t) => t -> [t] -> Bool
pertenece _ [] = False
pertenece e (x:xs) | e == x = True
                   | otherwise = pertenece e xs


--pred mismosElementos
mismosElementos :: (Eq t) => [t] -> [t] -> Bool
mismosElementos l1 l2 = (longitud l1 == longitud l2) && (mismaLista l1 l2 && mismaLista l2 l1)

mismaLista :: (Eq t) => [t] -> [t] -> Bool  --sin repetidos, no importa orden
mismaLista [] l2 = True
mismaLista (x:xs) l2 | pertenece x l2 = mismaLista xs l2
                     | otherwise = False

longitud :: [t] -> Integer
longitud [] = 0
longitud (x:xs) = 1 + longitud xs



--pred redSocialValida
redSocialValida :: RedSocial -> Bool
redSocialValida red = (usuariosValidos (usuarios (red))) && (relacionesValidas (usuarios (red)) (relaciones red)) && (publicacionesValidas (usuarios red) (publicaciones red))



--pred usuariosValidos
usuariosValidos :: [Usuario] -> Bool
usuariosValidos [] = True
usuariosValidos (u:us) = usuarioValido u && noHayIdsRepetidos (u:us) && usuariosValidos us


--pred usuarioValido
usuarioValido :: Usuario -> Bool
usuarioValido u = (idDeUsuario u) > 0 && (longitud (nombreDeUsuario u)) > 0


--pred noHayIdsRepetidos  
noHayIdsRepetidos :: [Usuario] -> Bool
noHayIdsRepetidos [] = True
noHayIdsRepetidos (u:us) = noHayIdRepetido u us && noHayIdsRepetidos us
  where
    noHayIdRepetido :: Usuario -> [Usuario] -> Bool
    noHayIdRepetido _ [] = True
    noHayIdRepetido u (u':us') = idDeUsuario u /= idDeUsuario u' && noHayIdRepetido u us'


--pred relacionesValidas
relacionesValidas :: [Usuario] -> [Relacion] -> Bool
relacionesValidas us rels | usuariosDeRelacionValidos us rels && relacionesAsimetricas rels && noHayRelacionesRepetidas rels = True
                          | otherwise = False


--pred usuariosDeRelacionValidos
usuariosDeRelacionValidos :: [Usuario] -> [Relacion] -> Bool
usuariosDeRelacionValidos us [] = True
usuariosDeRelacionValidos us ((u1, u2):rels) = pertenece u1 us && pertenece u2 us && u1 /= u2 && usuariosDeRelacionValidos us rels


--pred relacionesAsimetricas
relacionesAsimetricas :: [Relacion] -> Bool
relacionesAsimetricas [] = True
relacionesAsimetricas ((u1, u2):rels) = not (pertenece (u2, u1) rels) && relacionesAsimetricas rels


--pred noHayRelacionesRepetidas
noHayRelacionesRepetidas :: [Relacion] -> Bool
noHayRelacionesRepetidas [] = True
noHayRelacionesRepetidas [_] = True
noHayRelacionesRepetidas ((u1, u2):(u3, u4):rels) | ((idDeUsuario u1 /= idDeUsuario u3) || (idDeUsuario u2 /= idDeUsuario u4)) && noHayRelacionesRepetidas rels = True
                                                  | otherwise = False


--pred publicacionesValidas
publicacionesValidas :: [Usuario] -> [Publicacion] -> Bool
publicacionesValidas us pubs = (usuariosDePublicacionSonUsuariosDeRed us pubs) && (usuariosDeLikeDePublicacionSonUsuariosDeRed us pubs) && (noHayPublicacionesRepetidas pubs)



--pred UsuariosDePublicacionSonUsuariosDeRed
usuariosDePublicacionSonUsuariosDeRed :: [Usuario] -> [Publicacion] -> Bool
usuariosDePublicacionSonUsuariosDeRed us [] = True
usuariosDePublicacionSonUsuariosDeRed [] pubs = False
usuariosDePublicacionSonUsuariosDeRed us (pub:pubs) = pertenece (usuarioDePublicacion pub) us && usuariosDePublicacionSonUsuariosDeRed us pubs


--pred UsuariosDeLikeDePublicacionSonUsuariosDeRed
usuariosDeLikeDePublicacionSonUsuariosDeRed :: [Usuario] -> [Publicacion] -> Bool
usuariosDeLikeDePublicacionSonUsuariosDeRed us [] = True
usuariosDeLikeDePublicacionSonUsuariosDeRed us (pub:pubs) = usuariosLikeValidos us (likesDePublicacion pub) && usuariosDeLikeDePublicacionSonUsuariosDeRed us pubs


--pred usuariosLikeValidos
usuariosLikeValidos :: [Usuario] -> [Usuario] -> Bool
usuariosLikeValidos us [] = True
usuariosLikeValidos us (u:usl) = pertenece u us && usuariosLikeValidos us usl


--pred noHayPublicacionesRepetidas
noHayPublicacionesRepetidas :: [Publicacion] -> Bool
noHayPublicacionesRepetidas [] = True
noHayPublicacionesRepetidas [_] = True
noHayPublicacionesRepetidas (pub:pubs) = noSeRepiteConResto pub pubs && noHayPublicacionesRepetidas pubs
  where
    noSeRepiteConResto _ [] = True
    noSeRepiteConResto pub1 (pub2:resto) = (idDeUsuario (usuarioDePublicacion pub1) /= idDeUsuario (usuarioDePublicacion pub2) || textoDePublicacion pub1 /= textoDePublicacion pub2) &&
      noSeRepiteConResto pub1 resto && noSeRepiteConResto pub2 resto

-- aux
textoDePublicacion :: Publicacion -> String
textoDePublicacion (_, str, _) = str



--pred cadenaDeAmigos 
cadenaDeAmigos :: [Usuario] -> RedSocial -> Bool
cadenaDeAmigos [] red = True
cadenaDeAmigos (u1:u2:us) red = relacionadosDirecto u1 u2 red && cadenaDeAmigos (u2:us) red


--pred relacionadosDirecto
relacionadosDirecto :: Usuario -> Usuario -> RedSocial -> Bool
relacionadosDirecto u1 u2 red = (pertenece (u1, u2) (relaciones red)) || pertenece (u2, u1) (relaciones red)


--pred sonDeLaRed
sonDeLaRed :: RedSocial -> [Usuario] -> Bool
sonDeLaRed red (u:us) = pertenece u (usuarios red) && sonDeLaRed red us


--pred empiezaCon
empiezaCon :: (Eq t) => t -> [t] -> Bool
empiezaCon e xs = head xs == e


--pred terminaCon
terminaCon :: (Eq t) => t -> [t] -> Bool
terminaCon e [] = False
terminaCon e [x] = e == x
terminaCon e (x:xs) = terminaCon e xs


--pred sinRepetidos
sinRepetidos :: (Eq t) => [t] -> Bool
sinRepetidos [] = True
sinRepetidos [x] = True
sinRepetidos (x:xs) = not (pertenece x xs) && sinRepetidos xs




















































































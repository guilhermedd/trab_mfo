module jogo {
    type TipoPersonagem = Mago | Cacador | Druida

    type Monstro = {
        hp: int,
        dmg: int,
    }  

    type Personagem = {
        hp: int,
        dmg: int,
        tipo_personagem: TipoPersonagem
    }  

    pure val DADO = Set(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20)  

    pure val PERSONAGENS = Set(
        {hp: 20, dmg: 10, tipo_personagem: Druida },
        {hp: 20, dmg: 10, tipo_personagem: Cacador },
        {hp: 20, dmg: 10, tipo_personagem: Mago }
    )

    pure val MONSTROS = Set(
        {hp: 100, dmg: 10}
    )

    pure def dano_personagem(p: Personagem, d: int): Personagem = {
        { ...p, hp: p.hp - d }
    }    

    pure def dano_monstro(p: Monstro, d: int): Monstro = {
        { ...p, hp: p.hp - d }
    }    

    pure def aumenta_dano_monstro(m: Monstro): Monstro = {
        { ...m, dmg: m.dmg + 10}
    }

    action tackle(atingido: Personagem, d: int): bool = all {
        PERSONAGENS' = PERSONAGENS.map(p => p.dano_personagem(d))
    }

}
module jogo {
    import basicSpells.* from "../spells/basicSpells"
    // import rareSpells.* from "../spells/rareSpell"

    type TipoCriatura = Mago | Cacador | Druida | Monstro

    type Criatura = { 
        name: str,
        hp: int,
        dmg: int,
        numero_dado: int,
        ja_jogou: bool,
        valor_dado: int,
        tipo_criatura: TipoCriatura
    }

    pure val DADO = 1.to(20)
    
    pure val CRIATURAS = Set(
        { name: "Roz",              hp: 20,     dmg: 10, ja_jogou: false, valor_dado: -1, tipo_criatura: Mago       },
        { name: "Boo",              hp: 20,     dmg: 10, ja_jogou: false, valor_dado: -1, tipo_criatura: Cacador    },
        { name: "Sullivan",         hp: 20,     dmg: 10, ja_jogou: false, valor_dado: -1, tipo_criatura: Druida     },
        { name: "Mike wazousky",    hp: 100,    dmg: 10, ja_jogou: false, valor_dado: -1, tipo_criatura: Monstro    }
    )

    var criaturas: Criatura -> int
    var chave_max: Criatura -> int
    var atacante: Criatura -> int
    var alvo: Criatura

    pure def dano_criatura(p: Criatura, d: int): Criatura = {
        { ...p, hp: p.hp - d }
    }

    pure def update_dmg_monstro(m: Criatura): Criatura = {
        { ...m, dmg: 20}
    }

    pure def terminou_acao(c: Criatura): Criatura = {
        { ...c, ja_jogou: true}
    }
    
    // Ação: Monstro ataca o Criatura
    // action ataque(attacker: Criatura, receiver: Criatura): bool = all {
    //     criaturas' = criaturas.setBy(receiver.name, p => p.dano_criatura(attacker.dmg))
    // }

    // Ação: Ataque que decide o próximo movimento de acordo com quem ainda está vivo
    action batalha(attacker: Criatura, receiver: Criatura): bool = all {
        attacker != receiver,
        attacker.hp > 0, 
        receiver.hp > 0,
        // ataque(attacker, receiver),
        // terminou_acao(attacker),
    }

    // pure def map_dos_dados_rolados(dado: int): int -> Criatura = {
    //     CRIATURAS.map(c => dado -> c)
    // }

    action init = {
        pure val auxCreatures = CRIATURAS
        nondet personagem_inicial = CRIATURAS.filter(criatura => criatura.tipo_criatura != Monstro).oneOf()
        nondet monstro_inicial = CRIATURAS.filter(criatura => criatura.tipo_criatura == Monstro).oneOf()
        nondet dado_1 = DADO.oneOf()
        nondet dado_2 = DADO.oneOf()
        nondet dado_3 = DADO.oneOf()
        nondet dado_4 = DADO.oneOf()
        nondet criatura_aux = CRIATURAS.oneOf()
        nondet criatura1 = auxCreatures.filter(criatura => criatura.hp > 0).oneOf()
        nondet criatura2 = auxCreatures.exclude(Set(criatura1)).filter(criatura => criatura.hp > 0).oneOf()
        nondet criatura3 = auxCreatures.exclude(Set(criatura1, criatura2)).filter(criatura => criatura.hp > 0).oneOf()
        nondet criatura4 = auxCreatures.exclude(Set(criatura1, criatura2, criatura3)).filter(criatura => criatura.hp > 0).oneOf()
        pure val criatura_map = Set((criatura1 -> dado_1), (criatura2 -> dado_2), (criatura3 -> dado_3), (criatura4 -> dado_4))
        all {
            criaturas' = criatura_map, // criatura -> dado
            chave_max' = criaturas.fold((0, 0), (acc, criatura) => if (criaturas._2 > acc._2) criatura else acc),
            atacante' = chave_max._1,
            alvo' = if(atacante.tipo_criatura == Monstro) personagem_inicial else monstro_inicial,
            batalha(atacante, alvo),
            criaturas.exclude(Set(atacante._1 -> atacante._2)),
            update_dmg_monstro(monstro_inicial),
            criaturas' = criaturas.exclude(Set(chave_max))
        }
    }


    // Ação: Realiza um passo no combate
    action step = {
        nondet personagem_atacado = CRIATURAS.filter(criatura => criatura.tipo_criatura != Monstro and criatura.hp > 0).oneOf()
        nondet monstro_atacado = CRIATURAS.filter(criatura => criatura.tipo_criatura == Monstro).oneOf()
        nondet dado_1 = DADO.oneOf()
        nondet dado_2 = DADO.oneOf()
        nondet dado_3 = DADO.oneOf()
        nondet dado_4 = DADO.oneOf()
        nondet criatura_map = CRIATURAS.filter(criatura => criatura.hp > 0).setOfMaps(Set(dado_1, dado_2, dado_3, dado_4)).oneOf()
        // todos rolam os dados -> quem tirar o maior numero ataca primeiro -> escolher ataque -> atacar
        all {
            chave_max' = criaturas.fold((0, 0), (acc, criatura) => if (criaturas._2 > acc._2) criatura else acc),
            atacante' = chave_max._1,
            alvo' = if(atacante.tipo_criatura == Monstro) personagem_atacado else monstro_atacado,
            batalha(atacante, alvo),
            criaturas.exclude(Set(atacante._1 -> atacante._2)),
            criaturas' = criaturas.exclude(Set(chave_max)),
            criaturas' = if (criaturas.size() > 0) criaturas else criatura_map, 
        }
    }


    // Invariante: O monstro ou o Criatura ainda estão vivos
    // val inv = criaturas.values().exists(p => p.hp > 0) and monstro.hp > 0
}
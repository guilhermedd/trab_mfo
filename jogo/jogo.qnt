module jogo {
    import basicSpells.* from "../spells/basicSpells"
    // import rareSpells.* from "../spells/rareSpell"

    type TipoCriatura = Mago | Cacador | Druida | Monstro
    type AtaqueEspecial = Cegueira | TransformacaoSelvagem | Ilusao | Paralisia | Nenhum


    type Criatura = { 
        name: str,
        hp: int,
        dmg: int,
        numero_dado: int,
        ja_jogou: bool,
        tipo_criatura: TipoCriatura,
        ataque_especial: AtaqueEspecial,
        efeito_aplicado: AtaqueEspecial
    }

    pure val DADO = 1.to(20)
    
    pure val CRIATURAS = Set(
        { name: "Roz",              hp: 20,     dmg: 10, ja_jogou: false, tipo_criatura: Mago,      ataque_especial: Ilusao,                efeito_aplicado: Nenhum },
        { name: "Boo",              hp: 20,     dmg: 10, ja_jogou: false, tipo_criatura: Cacador,   ataque_especial: Cegueira,              efeito_aplicado: Nenhum },
        { name: "Sullivan",         hp: 20,     dmg: 10, ja_jogou: false, tipo_criatura: Druida,    ataque_especial: TransformacaoSelvagem, efeito_aplicado: Nenhum },
        { name: "Mike wazousky",    hp: 100,    dmg: 10, ja_jogou: false, tipo_criatura: Monstro,   ataque_especial: Paralisia,             efeito_aplicado: Nenhum }
    )

    var crias: Criatura -> int
    var chave_max: Set[Criatura -> int]
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
    action ataque(attacker: Criatura, receiver: Criatura): bool = all {
        // crias' = crias.setBy(receiver, p => receiver.dano_criatura(attacker.dmg)),
        crias' = crias
    }

    // Ação: Ataque que decide o próximo movimento de acordo com quem ainda está vivo
    action batalha(attacker: Criatura, receiver: Criatura): bool ={
        all {
            attacker != receiver,
            attacker.hp > 0, 
            receiver.hp > 0,
            // ataque(attacker, receiver),
            // terminou_acao(attacker),
        }
    }

    action usar_habilidade(attacker: Criatura, receiver: Criatura): bool = {
        all {
            crias' = crias.setBy(c, receiver => c.tomar_habilidade(actual_damage)),
            last_attack' = Some({ attacker: attacker, receiver: receiver, attack: "Elemental", damage: actual_damage }),
        }
    }

    action remover_paralisia(removedor: Criatura, paralisado: Criatura): Criatura {

    }

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
        nondet criatura2 = auxCreatures.setRemove(Set(criatura1)).filter(criatura => criatura.hp > 0).oneOf()
        nondet criatura3 = auxCreatures.exclude(Set(criatura1, criatura2)).filter(criatura => criatura.hp > 0).oneOf()
        nondet criatura4 = auxCreatures.exclude(Set(criatura1, criatura2, criatura3)).filter(criatura => criatura.hp > 0).oneOf()
        pure val criatura_map = Set((criatura1 -> dado_1), (criatura2 -> dado_2), (criatura3 -> dado_3), (criatura4 -> dado_4))
        all {
            chave_max' = criatura_map.fold((0, 0), (acc, criatura) => if (criatura._2 > acc._2) criatura else acc),
            atacante' = chave_max._1,
            alvo' = if(atacante.tipo_criatura == Monstro) personagem_inicial else monstro_inicial,
            batalha(atacante, alvo),
            crias' = criatura_map.exclude(Set(atacante._1 -> atacante._2)).setToMap(),
            update_dmg_monstro(monstro_inicial),
        }
    }


    // Ação: Realiza um passo no combate
    action step = {
        // nondet personagem_atacado = CRIATURAS.filter(criatura => criatura.tipo_criatura != Monstro and criatura.hp > 0).oneOf()
        // nondet monstro_atacado = CRIATURAS.filter(criatura => criatura.tipo_criatura == Monstro).oneOf()
        // nondet dado_1 = DADO.oneOf()
        // nondet dado_2 = DADO.oneOf()
        // nondet dado_3 = DADO.oneOf()
        // nondet dado_4 = DADO.oneOf()
        // nondet criatura_map = CRIATURAS.filter(criatura => criatura.hp > 0).setOfMaps(Set(dado_1, dado_2, dado_3, dado_4)).oneOf()
        // // todos rolam os dados -> quem tirar o maior numero ataca primeiro -> escolher ataque -> atacar
        all {
            chave_max' = crias.fold((0, 0), (acc, criatura) => if (criatura._2 > acc._2) criatura else acc),
            // atacante' = chave_max._1,
            // // alvo' = if(atacante.tipo_criatura == Monstro) personagem_atacado else monstro_atacado,
            // batalha(atacante, alvo),
            // crias.exclude(Set(atacante._1 -> atacante._2)),
            // crias' = crias.exclude(Set(chave_max)),
            // crias' = if (crias.size() > 0) crias else criatura_map, 
        }
    }


    // Invariante: O monstro ou o Criatura ainda estão vivos
    // val inv = crias.values().exists(p => p.hp > 0) and monstro.hp > 0
}
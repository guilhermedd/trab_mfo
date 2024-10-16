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
        efeito_aplicado: AtaqueEspecial,
    }

    pure val DADO = 1.to(20)
    pure val escolhas_de_ataques_para_as_batalhas = Set("ataque_simples", "usar_habilidade", "curar_paralisia")
    
    pure val CRIATURAS = Set(
        { name: "Roz",              hp: 20,     dmg: 10, ja_jogou: false, tipo_criatura: Mago,      ataque_especial: Ilusao,                efeito_aplicado: Nenhum },
        { name: "Boo",              hp: 20,     dmg: 10, ja_jogou: false, tipo_criatura: Cacador,   ataque_especial: Cegueira,              efeito_aplicado: Nenhum },
        { name: "Sullivan",         hp: 20,     dmg: 10, ja_jogou: false, tipo_criatura: Druida,    ataque_especial: TransformacaoSelvagem, efeito_aplicado: Nenhum },
        { name: "Mike wazousky",    hp: 0,    dmg: 10, ja_jogou: false, tipo_criatura: Monstro,   ataque_especial: Paralisia,             efeito_aplicado: Nenhum }
    )

    var crias: Criatura -> int
    var acabou_de_atacar: Set[Criatura -> int]
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
    pure def aplica_efeito(atacante: Criatura, tomador: Criatura): Criatura = {
        { ...tomador, efeito_aplicado: atacante.ataque_especial}
    }
    pure def remove_efeito(c: Criatura): Criatura = {
        { ...c, efeito_aplicado: Nenhum}
    }
    // Ação: Monstro ataca o Criatura
    action ataque_simples(attacker: Criatura, receiver: Criatura): bool = {
        // crias' = crias.setBy(receiver, p => receiver.dano_criatura(attacker.dmg)),
        val dado = crias.get(receiver)
        val aux = crias.mapRemove(receiver)

        all {
            dano_criatura(dado, attacker.dmg),    
            crias' = aux.put(receiver, dado),
        }
    }
    pure def curar_paralisia(c : Criatura): Criatura = {
        { ...c, hp: c.hp + 10}
        all {
            crias' = crias.map(c => remove_efeito(c)),
        }

    }

    // Ação: Ataque que decide o próximo movimento de acordo com quem ainda está vivo
    action batalha(attacker: Criatura, receiver: Criatura): bool ={
        nondet tipo_de_ataque = escolhas_de_ataques_para_as_batalhas.oneOf()
        val tem_alguem_paralisado
        if(crias.keys().filter(c => c.efeito_aplicado == Paralisia).size() >= 1){
            tem_alguem_paralisado = true
        } else {
            tem_alguem_paralisado = false
        }
        nondet tipo_de_ataque = .oneOf()
        all {
            attacker != receiver,
            attacker.hp > 0, 
            receiver.hp > 0,
            match tipo_de_ataque {
                | "ataque_simples" => ataque_simples(attacker, receiver)
                | "usar_habilidade" => usar_habilidade(attacker, receiver)
                | "curar_paralisia" => match tem_alguem_paralisado {
                    | true => curar_paralisia(receiver)
                    | false => usar_habilidade(attacker, receiver)
                }
            },            
            terminou_acao(attacker),
        }
    }

    action usar_habilidade(attacker: Criatura, receiver: Criatura): bool = {
        val dado_cria_tomante = crias.get(receiver)
        val aux = crias.mapRemove(receiver)
        all {
            aplica_efeito(attacker, receiver),
            crias' = aux.put(receiver, dado_cria_tomante),
        }
    }

    // action remover_paralisia(removedor: Criatura, paralisado: Criatura): Criatura = {
    //     crias' = crias
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
        nondet criatura2 = auxCreatures.setRemove(Set(criatura1)).filter(criatura => criatura.hp > 0).oneOf()
        nondet criatura3 = auxCreatures.exclude(Set(criatura1, criatura2)).filter(criatura => criatura.hp > 0).oneOf()
        nondet criatura4 = auxCreatures.exclude(Set(criatura1, criatura2, criatura3)).filter(criatura => criatura.hp > 0).oneOf()
        pure val criaturas_disponiveis = Set((criatura1 -> dado_1), (criatura2 -> dado_2), (criatura3 -> dado_3), (criatura4 -> dado_4))

        all {
            acabou_de_atacar' = criaturas_disponiveis.fold((0, 0), (acc, criatura) => if (criatura._2 > acc._2) criatura else acc),
            atacante' = acabou_de_atacar._1,
            alvo' = if(atacante.tipo_criatura == Monstro) personagem_inicial else monstro_inicial,
            batalha(atacante, alvo),
            crias' = criaturas_disponiveis.exclude(Set(acabou_de_atacar._1 -> acabou_de_atacar._2)).setToMap(),
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
        // nondet criaturas_disponiveis = CRIATURAS.filter(criatura => criatura.hp > 0).setOfMaps(Set(dado_1, dado_2, dado_3, dado_4)).oneOf()
        // // todos rolam os dados -> quem tirar o maior numero ataca primeiro -> escolher ataque -> atacar
        all {
            acabou_de_atacar' = crias.fold((0, 0), (acc, criatura) => if (criatura._2 > acc._2) criatura else acc),
            // atacante' = acabou_de_atacar._1,
            // // alvo' = if(atacante.tipo_criatura == Monstro) personagem_atacado else monstro_atacado,
            // batalha(atacante, alvo),
            // crias.exclude(Set(atacante._1 -> atacante._2)),
            // crias' = crias.exclude(Set(acabou_de_atacar)),
            // crias' = if (crias.size() > 0) crias else criaturas_disponiveis, 
        }
    }


    // Invariante: O monstro ou o Criatura ainda estão vivos
    // val inv = crias.values().exists(p => p.hp > 0) and monstro.hp > 0
}
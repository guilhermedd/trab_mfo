module jogo {
    import basicSpells.* from "../spells/basicSpells"
    // import rareSpells.* from "../spells/rareSpell"

    type TipoCriatura = Mago | Cacador | Druida | Monstro | Animal
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

    pure val CRIATURAS = Set(
        { name: "Roz",              hp: 20,     dmg: 10, numero_dado: 0, ja_jogou: false, tipo_criatura: Mago,      ataque_especial: Ilusao,                efeito_aplicado: Nenhum },
        { name: "Boo",              hp: 20,     dmg: 10, numero_dado: 0, ja_jogou: false, tipo_criatura: Cacador,   ataque_especial: Cegueira,              efeito_aplicado: Nenhum },
        { name: "Sullivan",         hp: 20,     dmg: 10, numero_dado: 0, ja_jogou: false, tipo_criatura: Druida,    ataque_especial: TransformacaoSelvagem, efeito_aplicado: Nenhum },
        { name: "Mike wazousky",    hp: 100,    dmg: 10, numero_dado: 0, ja_jogou: false, tipo_criatura: Monstro,   ataque_especial: Paralisia,             efeito_aplicado: Nenhum }
    )

    pure val OVELHA = {name: "Shaun", hp: 1, dmg: 0, numero_dado: 0, ja_jogou: false, tipo_criatura: Animal, ataque_especial: Nenhum, efeito_aplicado: Nenhum}
    
    pure val DADO = 1.to(20)
    pure val escolhas_de_ataques_para_as_batalhas = Set("ataque_simples", "usar_habilidade", "curar_paralisia")

    var crias: Criatura -> int
    var criatura_dado: int -> Criatura
    var acabou_de_atacar: Set[Criatura -> int]
    var atacante: Criatura -> int
    var alvo: Criatura

    pure def dano_criatura(p: Criatura, d: int): Criatura = {
        { ...p, hp: p.hp - d }
    }
    pure def curar_paralisia(c: Criatura): Criatura = {
        { ...c, efeito_aplicado: Nenhum}
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
    pure def inverter_dado(c: Criatura): Criatura = {
        { ...c, numero_dado: -c.numero_dado}
    }
    pure def atualiza_map(crias: int -> Criatura): int -> Criatura = {
        crias.values().map(cria => (cria.numero_dado, cria)).setToMap() 
    }
    pure def atualizar_atributos_do_monstro (crias: int -> Criatura, acabou_de_atacar: Criatura, monstro: Criatura): int -> Criatura = {
        pure val aux = crias.set(acabou_de_atacar.numero_dado, inverter_dado(acabou_de_atacar))
        aux.set(monstro.numero_dado, update_dmg_monstro(monstro))
    }


    // Ação: Monstro ataca o Criatura
    action ataque_simples(atacante: Criatura, receiver: Criatura): bool = {
        pure val nova_cria = dano_criatura(receiver, atacante.dmg)
        all {
            atacante.tipo_criatura != Monstro,
            criatura_dado' = criatura_dado.set(nova_cria.numero_dado, nova_cria),
        }
    }

    pure def adicionar_dado(criatura: Criatura, dado: int): Criatura = {
        {...criatura, numero_dado: dado}
    }

    action desparalizar(atacante: Criatura, receiver: Criatura): bool = {
        nondet curado = criatura_dado.values().filter(c => c.tipo_criatura != Monstro and c.efeito_aplicado == Paralisia).oneOf()
        val aux = remove_efeito(curado)
        all {
            atacante.tipo_criatura != Monstro,
            criatura_dado' = criatura_dado.set(receiver.numero_dado, aux),
        }
    }

    action usar_habilidade(atacante: Criatura, receiver: Criatura): bool = {
        val dado_cria_tomante = crias.get(receiver)
        val aux = crias.mapRemove(receiver)
        all {
            aplica_efeito(atacante, receiver),
            crias' = aux.put(receiver, dado_cria_tomante),
        }
    }

    action escolha_aleatoria(atacante: Criatura, recebedor: Criatura): bool = any {
        desparalizar(atacante, recebedor),
        usar_habilidade(atacante, recebedor),
        ataque_simples(atacante, recebedor),
    }

    action ilusao(atacante: Criatura): bool = all {
        criatura_dado' = criatura_dado.put(OVELHA.numero_dado, OVELHA),
    }

    // Ação: Ataque que decide o próximo movimento de acordo com quem ainda está vivo
    action batalha(atacante: Criatura, receiver: Criatura): bool ={
        nondet tipo_de_ataque = escolhas_de_ataques_para_as_batalhas.oneOf()
        val tem_alguem_paralisado = criatura_dado.values().filter(c => c.tipo_criatura != Monstro).forall(c => c.efeito_aplicado != Paralisia)
        all {
            atacante.name != "Shaun",
            atacante.hp > 0, 
            receiver.hp > 0,
            atacante.ja_jogou == false,
            escolha_aleatoria(atacante, receiver),        
            criatura_dado' = criatura_dado.set(atacante.numero_dado, terminou_acao(atacante)),
        }
        // Druida usa habilidade em si | Montro ataca Animal | Mago adiciona ovelha | Cacador cega
        // Se atacante for Monstro, ele ataca o Animal, nao pode usar habilidade
        // No usar habilidade, se atacante.tipo_criatura == Druida, atacante = recebedor
        // Se Cacador usar habilidade, recebedor == Monstro
        // All: Se atacante = Monstro, Recebedor != Monstro
    }

    action init = {
        // pure val auxCreatures = CRIATURAS
        pure val mago =     CRIATURAS.filter(criatura => criatura.tipo_criatura == Mago)
        pure val cacador =  CRIATURAS.filter(criatura => criatura.tipo_criatura == Cacador)
        pure val druida =   CRIATURAS.filter(criatura => criatura.tipo_criatura == Druida)
        pure val monstro =  CRIATURAS.filter(criatura => criatura.tipo_criatura == Monstro)

        nondet personagem_inicial = CRIATURAS.filter(criatura => criatura.tipo_criatura != Monstro).oneOf()
        nondet monstro_inicial = CRIATURAS.filter(criatura => criatura.tipo_criatura == Monstro).oneOf()
        
        nondet dado_mago = DADO.oneOf()
        nondet dado_cacador = DADO.setRemove(dado_mago).oneOf()
        nondet dado_druida = DADO.setRemove(dado_mago).setRemove(dado_cacador).oneOf()
        nondet dado_monstro = DADO.setRemove(dado_mago).setRemove(dado_cacador).setRemove(dado_druida).oneOf()

        nondet criatura_aux = CRIATURAS.oneOf()


        pure val aux = Set({...mago, numero_dado: dado_mago}, {...cacador, numero_dado: dado_cacador}, {...druida, numero_dado: dado_druida}, {...monstro, numero_dado: dado_monstro})

        pure val aux2 = criatura_dado' = aux.map(cria => (cria.numero_dado, cria)).setToMap()  // mudar criaturas
        all {
            acabou_de_atacar' = aux2.values().fold(personagem_inicial, (acc, criatura) => if (criatura.numero_dado > acc.numero_dado) criatura else acc),
            alvo' = if(acabou_de_atacar.tipo_criatura == Monstro) personagem_inicial else monstro_inicial,
            batalha(acabou_de_atacar, alvo),
            // crias' = criaturas_disponiveis.exclude(Set(acabou_de_atacar._1 -> acabou_de_atacar._2)).setToMap(),
            criatura_dado' = atualizar_atributos_do_monstro(aux2, acabou_de_atacar, monstro),
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
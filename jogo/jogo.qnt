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
    pure val URSO = {name: "Pooh", hp: 60, dmg: 20, numero_dado: 0, ja_jogou: false, tipo_criatura: Animal, ataque_especial: Nenhum, efeito_aplicado: Nenhum}

    pure val DADO = 1.to(20)
    pure val escolhas_de_ataques_para_as_batalhas = Set("ataque_simples", "usar_habilidade", "curar_paralisia")

    // var crias: Criatura -> int
    var dado_criaturas: int -> Criatura
    var acabou_de_atacar: Criatura -> int
    var atacante: Criatura -> int
    var alvo: Criatura
    var druida_backup_pra_quando_acabar_a_transformacao: Criatura
    var proximo_ataque: str


//-----------------------------------------------------FUNCOES AUXILIARES--------------------------------------------------------------------
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
    pure def inverter_dado(c: Criatura): Criatura = {
        { ...c, numero_dado: -c.numero_dado}
    }
    pure def atualiza_map(crias: int -> Criatura): int -> Criatura = {
        crias.values().map(cria => (cria.numero_dado, cria)).setToMap() 
    }
    pure def atualizar_atributos_das_criaturas (crias: int -> Criatura, acabou_de_atacar: Criatura, monstro: Criatura): int -> Criatura = {
        pure val aux = crias.set(abs(acabou_de_atacar.numero_dado), inverter_dado(acabou_de_atacar))
        aux.set(abs(monstro.numero_dado), update_dmg_monstro(monstro)) // perguntar para a prof se pode fazer isso: isso vai retornar um Map?
    }
    pure def adicionar_dado(criatura: Criatura, dado: int): Criatura = {
        {...criatura, numero_dado: dado}
    }
    



//-----------------------------------------------------HABILIDADES ESPECIAIS--------------------------------------------------------------------
    action paralisia(atacante: Criatura, tomador: Criatura): bool = all {
        atacante.tipo_criatura == Monstro,
        tomador.tipo_criatura != Monstro,
        dado_criaturas' = dado_criaturas.set(abs(tomador.numero_dado), aplica_efeito(atacante, tomador)),
    }
    action ilusao(atacante: Criatura): bool = all {
        atacante.tipo_criatura == Mago,
        dado_criaturas' = dado_criaturas.put(OVELHA.numero_dado, OVELHA),
    }
    action transformacao_selvagem(atacante: Criatura): bool = 
        all {
        atacante.tipo_criatura == Druida,
        druida_backup_pra_quando_acabar_a_transformacao' = atacante,                                        
        dado_criaturas' = dado_criaturas.set(abs(atacante.numero_dado), URSO), //tem a funcao desvirar_urso_de_volta_para_druida
    }
    action cegueira(atacante: Criatura, tomador: Criatura): bool = all {
        atacante.tipo_criatura == Cacador,
        tomador.tipo_criatura == Monstro,
        tomador' = aplica_efeito(atacante, tomador),
    }


//-----------------------------------------------------FUNCOES BASICAS--------------------------------------------------------------------
    action ataque_simples(atacante: Criatura, receiver: Criatura): bool = {
        pure val nova_cria = dano_criatura(receiver, atacante.dmg)
        all {
            atacante.tipo_criatura != receiver.tipo_criatura,
            dado_criaturas' = dado_criaturas.set(abs(nova_cria.numero_dado), nova_cria),
        }
    }
    action desparalizar(atacante: Criatura, receiver: Criatura): bool = {
        all { // Pedir para a prof se pode fazer isso
            atacante.efeito_aplicado != Paralisia,
            dado_criaturas.values().exists(c => c.tipo_criatura != Monstro and c.efeito_aplicado == Paralisia) == true,
            curar(receiver),
        }        
    }
    action curar(receiver: Criatura): bool ={ // faz parte de desparalizar
        nondet curado = dado_criaturas.values().filter(c => c.tipo_criatura != Monstro and c.efeito_aplicado == Paralisia).oneOf()
        val aux = remove_efeito(curado)
        all {
            curado != Set(),
            atacante.tipo_criatura != Monstro,
            dado_criaturas' = dado_criaturas.set(abs(receiver.numero_dado), aux),
        }
    }
    action desvirar_urso_de_volta_para_druida(c: Criatura): bool = all{
        c.name == "Pooh",
        dado_criaturas' = dado_criaturas.set(abs(c.numero_dado), druida_backup_pra_quando_acabar_a_transformacao),
    }
    action usar_habilidade(atacante: Criatura, receiver: Criatura): bool = all {
        atacante.efeito_aplicado != Paralisia,
        if (atacante.tipo_criatura == Mago) { ilusao(atacante) }
        else if (atacante.tipo_criatura == Druida) { transformacao_selvagem(atacante) }
        else if (atacante.tipo_criatura == Cacador) { cegueira(atacante, receiver) }
        else { paralisia(atacante, receiver) } // Monstro
    }
    



//-----------------------------------------------------FUNCOES PRINCIPAIS--------------------------------------------------------------------

    action batalha(atacante: Criatura, receiver: Criatura): bool = {
        //nondet tipo_de_ataque = escolhas_de_ataques_para_as_batalhas.oneOf()
        val tem_alguem_paralisado = dado_criaturas.values().filter(c => c.tipo_criatura != Monstro).forall(c => c.efeito_aplicado != Paralisia)
        val existe_animal = dado_criaturas.values().exists(c => c.tipo_criatura == Animal)
        nondet proximo_ataque = escolhas_de_ataques_para_as_batalhas.oneOf()
        if (existe_animal and atacante.tipo_criatura == Monstro) {  // Pedir para a prof se pode fazer isso
            nondet recebedor = dado_criaturas.values().filter(c => c.tipo_criatura == Animal).oneOf()
        } else {
            nondet recebedor = dado_criaturas.values().filter(c => c.tipo_criatura == Animal).oneOf()
        }
        all {
            //garantias
            atacante.name != "Shaun",
            atacante.hp > 0, 
            receiver.hp > 0, // Pedir para a prof se pode fazer isso 
            atacante.ja_jogou == false,
            //ações
            if(proximo_ataque == "curar_paralisia") {desparalizar(atacante, receiver)} 
            else if(proximo_ataque == "usar_habilidade") {usar_habilidade(atacante, receiver)} 
            else {ataque_simples(atacante, receiver)},
            dado_criaturas' = dado_criaturas.set(abs(atacante.numero_dado), terminou_acao(atacante)),
        }
        // Druida usa habilidade em si✅ | Montro ataca AnimalAnimal🆘 | Mago adiciona ovelhaovelha✅ | Cacador cegacega✅
        // Se atacante for Monstro, ele ataca o Animal, nao pode usar habilidade🆘
        // No usar habilidade, se atacante.tipo_criatura == Druida, atacante = recebedor🆘
        // Se Cacador usar habilidade, recebedor == Monstro
        // All: Se atacante = Monstro, Recebedor != MonstroMonstro✅
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

        pure val aux = Set({...mago, numero_dado: dado_mago}, {...cacador, numero_dado: dado_cacador}, {...druida, numero_dado: dado_druida}, {...monstro, numero_dado: dado_monstro})

        pure val aux2 = aux.map(cria => (cria.numero_dado, cria)).setToMap()  // mudar criaturas
        all {
            acabou_de_atacar' = aux2.values().fold(personagem_inicial, (acc, criatura) => if (criatura.numero_dado > acc.numero_dado) criatura else acc),
            alvo' = if(acabou_de_atacar.tipo_criatura == Monstro) personagem_inicial else monstro_inicial,
            batalha(acabou_de_atacar, alvo),
            dado_criaturas' = atualizar_atributos_das_criaturas(aux2, acabou_de_atacar, monstro),
            CRIATURAS' = dado_criaturas.values()
        }
    }


    // Ação: Realiza um passo no combate
    action step = {
        nondet personagem_inicial = CRIATURAS.filter(criatura => criatura.tipo_criatura != Monstro).oneOf()
        nondet monstro_inicial = CRIATURAS.filter(criatura => criatura.tipo_criatura == Monstro).oneOf()
        if(CRIATURAS.exist(c => c.numero_dado > 0)){
            personagem_inicial = CRIATURAS.filter(criatura => criatura.tipo_criatura != Monstro).oneOf() // Pedir para a prof se pode fazer isso: redefinir nondet
        }
        else {
            pure val aux = CRIATURAS.map(cria => (cria.numero_dado, inverter_dado(cria))).setToMap()
        }
        all {
            acabou_de_atacar' = aux.values().fold(personagem_inicial, (acc, criatura) => if (criatura.numero_dado > acc.numero_dado) criatura else acc), // pedir para a prof se as verificacoes em batalha ja sao o suficiente
            alvo' = if(acabou_de_atacar.tipo_criatura == Monstro) personagem_inicial else monstro_inicial,
            batalha(acabou_de_atacar, alvo),
            dado_criaturas' = atualizar_atributos_das_criaturas(CRIATURAS, acabou_de_atacar, monstro),
            CRIATURAS' = dado_criaturas.values()
        }
    }

    val o_monstro_nao_morre = dado_criaturas.values().filter(c => c.tipo_criatura == Monstro).forall(c => c.hp > 0)
    val os_personagens_nao_morrem = dado_criaturas.values().filter(c => c.tipo_criatura != Monstro).forall(c => c.hp > 0)

     // 99 = 4  20  10  9



//-----------------------------------------------------INVARIANTES--------------------------------------------------------------------

    // Invariante: O monstro ou o Criatura ainda estão vivos
    // val inv = crias.values().exists(p => p.hp > 0) and monstro.hp > 0

}




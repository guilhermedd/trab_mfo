module jogo {
    import basicSpells.* from "../spells/basicSpells"
    // import rareSpells.* from "../spells/rareSpell"

    type TipoCriatura = Mago | Cacador | Druida | Monstro | Animal
    type AtaqueEspecial = Cegueira | TransformacaoSelvagem | Ilusao | Paralisia | Nenhum
    type Turno = Um | Dois | Tres


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
    var acabou_de_atacar: Criatura
    // var atacante: Criatura -> int
    var alvo: Criatura
    var druida_backup_pra_quando_acabar_a_transformacao: Criatura
    var proximo_ataque: str
    var idx_monstro: int
    var monstrengo: Criatura


//-----------------------------------------------------FUNCOES AUXILIARES--------------------------------------------------------------------
    pure def dano_criatura(p: Criatura, d: int): Criatura = {
        { ...p, hp: p.hp - d }
    }
    pure def update_dmg_monstro(m: Criatura): Criatura = {
        { ...m, dmg: 20}
    }
    pure def terminou_acao(c: Criatura): Criatura = {
        val r = { ...c, ja_jogou: not(c.ja_jogou)}
        { ...r, numero_dado: -r.numero_dado}
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
        crias.setBy(abs(acabou_de_atacar.numero_dado), c => inverter_dado(c))
            .setBy(abs(monstro.numero_dado), m => update_dmg_monstro(m)) // perguntar para a prof se pode fazer isso: isso vai retornar um Map?
    }
    pure def adicionar_dado(criatura: Criatura, dado: int): Criatura = {
        {...criatura, numero_dado: dado}
    }


//-----------------------------------------------------FIM DE ACAO--------------------------------------------------------------------
    // action remover_ovelha(): bool = all {
    //     dado_criaturas' = dado_criaturas.mapRemove(0)
    // }
    def remover_cegueira(c: Criatura): int -> Criatura = {
        dado_criaturas.setBy(abs(c.numero_dado), cri => remove_efeito(cri))
    }
    def desvirar_urso_de_volta_para_druida(c: Criatura): int -> Criatura = {
        dado_criaturas.setBy(abs(c.numero_dado), cri => druida_backup_pra_quando_acabar_a_transformacao)
    }
    

//-----------------------------------------------------HABILIDADES ESPECIAIS--------------------------------------------------------------------
    action paralisia(atacante: Criatura, tomador: Criatura): bool = all {
        atacante.tipo_criatura == Monstro,
        tomador.tipo_criatura != Monstro,
        dado_criaturas' = dado_criaturas.setBy(abs(tomador.numero_dado), c => aplica_efeito(atacante, tomador))
            .setBy(abs(atacante.numero_dado), c => inverter_dado(c))
            .setBy(abs(atacante.numero_dado), m => update_dmg_monstro(m)),
    }
    action ilusao(atacante: Criatura, id_monstro: int): bool = {
        all {
            atacante.tipo_criatura == Mago,

            dado_criaturas' = dado_criaturas.put(OVELHA.numero_dado, OVELHA)
                .setBy(abs(atacante.numero_dado), c => inverter_dado(c))
                .setBy(abs(id_monstro), m => update_dmg_monstro(m)),
        }
    }
    action transformacao_selvagem(atacante: Criatura, id_monstro: int): bool = {
        all {
            atacante.tipo_criatura == Druida,
            // druida_backup_pra_quando_acabar_a_transformacao' = atacante,
            dado_criaturas' = dado_criaturas.setBy(abs(atacante.numero_dado), c => URSO)
                .setBy(abs(atacante.numero_dado), c => inverter_dado(c))
                .setBy(abs(id_monstro), m => update_dmg_monstro(m)), //tem a funcao desvirar_urso_de_volta_para_druida
        }
    }
    action cegueira(atacante: Criatura, tomador: Criatura, id_monstro: int): bool = all {
        atacante.tipo_criatura == Cacador,
        tomador.tipo_criatura == Monstro,
        // tomador' = aplica_efeito(atacante, tomador),
        dado_criaturas' = dado_criaturas.setBy(abs(tomador.numero_dado), c => aplica_efeito(atacante, tomador))
            .setBy(abs(atacante.numero_dado), c => inverter_dado(c))
            .setBy(abs(id_monstro), m => update_dmg_monstro(m)),
    }


//-----------------------------------------------------FUNCOES BASICAS--------------------------------------------------------------------
    action ataque_simples(atacante: Criatura, receiver: Criatura, id_monstro: int): bool = {
        pure val nova_cria = dano_criatura(receiver, atacante.dmg)
        all {
            atacante.tipo_criatura != receiver.tipo_criatura,
            dado_criaturas' = dado_criaturas.setBy(abs(atacante.numero_dado), c => nova_cria),
        }
    }
    action desparalizar(atacante: Criatura, receiver: Criatura): bool = {
        nondet curado = dado_criaturas.values().filter(c => c.tipo_criatura != Monstro and c.efeito_aplicado == Paralisia).oneOf()
        val aux = remove_efeito(curado)
        all { // Pedir para a prof se pode fazer isso
            atacante.efeito_aplicado != Paralisia,
            atacante.tipo_criatura != Monstro,
            dado_criaturas.values().exists(c => c.tipo_criatura != Monstro and c.efeito_aplicado == Paralisia) == true,
            // curar(atacante, receiver),
            dado_criaturas' = dado_criaturas.setBy(abs(receiver.numero_dado), c => aux),

        }        
    }

    action usar_habilidade(atacante: Criatura, receiver: Criatura, id_monstro: int): bool = all {
        atacante.efeito_aplicado != Paralisia,
        dado_criaturas.values().size() > 0,
        any {
            ilusao(atacante, id_monstro),
            transformacao_selvagem(atacante, id_monstro),
            cegueira(atacante, receiver, id_monstro),
            paralisia(atacante, receiver),
        }
    }

// Ovelha: toda vez que é o turno do Mago, ovelha sai
// Quando for o turno do Druida: se ele estiver em modo Urso e atacou=true -> ele ataca primeiro e depois desvira urso | 
// Talvez adicionar: esta_paralisado ?
// 

//-----------------------------------------------------FUNCOES PRINCIPAIS--------------------------------------------------------------------

    action batalha(atacante: Criatura, receiver: Criatura, id_monstro: int, dados_das_criaturas: int -> Criatura): bool = { // tirar receiver
        // Fazer o que for preciso antes da batalha

        val dados_crias = dados_das_criaturas.values()
        val efeito_removido_case = match atacante.tipo_criatura {
            | Monstro   => remover_cegueira(atacante) // Se puder, remover a cegueira
            | Mago      => dado_criaturas.mapRemove(0)
            | Animal    => desvirar_urso_de_volta_para_druida(atacante)
            | Cacador   => dado_criaturas
            | Druida    => dado_criaturas
        }

        //nondet tipo_de_ataque = escolhas_de_ataques_para_as_batalhas.oneOf()
        val tem_alguem_paralisado = dados_crias.filter(c => c.tipo_criatura != Monstro).forall(c => c.efeito_aplicado != Paralisia)
        val existe_animal = dados_crias.exists(c => c.tipo_criatura == Animal)
        nondet proximo_ataque = escolhas_de_ataques_para_as_batalhas.oneOf()
        nondet escolha = Set(Um, Dois, Tres).oneOf()
        
        
        val recebedor =
            if (existe_animal and atacante.tipo_criatura == Monstro) {  // Pedir para a prof se pode fazer isso
                dados_crias.filter(c => c.tipo_criatura == Animal)
            } else {
                dados_crias.filter(c => c.name == receiver.name )
            }

        nondet recebedor_rng = recebedor.oneOf()
        all { // perguntar pra profa
            //garantias
            not(empty(dados_crias.filter(c => c.name == receiver.name ))),
            atacante.name != "Shaun",
            atacante.hp > 0, 
            recebedor_rng.hp > 0, // Pedir para a prof se pode fazer isso 
            atacante.ja_jogou == false,
            dado_criaturas.values().size() > 0,
            if (escolha == Um) {
                desparalizar(atacante, recebedor_rng)
            } else if (escolha == Dois) {
                usar_habilidade(atacante, recebedor_rng, id_monstro)
            } else {
                ataque_simples(atacante, recebedor_rng, id_monstro)
            }
            //ações
            
            // dado_criaturas' = efeito_removido_case.set(abs(atacante.numero_dado), terminou_acao(atacante)),
        }
        // Montro ataca AnimalAnimal🆘 | 
        // Se atacante for Monstro, ele ataca o Animal, nao pode usar habilidade🆘
        // No usar habilidade, se atacante.tipo_criatura == Druida, atacante = recebedor🆘
    }

    action init = {
        // TODO: adicionar logica de rounds: cegueira, urso e ovelha duram 1 round
        // pure val auxCreatures = CRIATURAS
        nondet mago =     CRIATURAS.filter(criatura => criatura.tipo_criatura == Mago).oneOf()
        nondet cacador =  CRIATURAS.filter(criatura => criatura.tipo_criatura == Cacador).oneOf()
        nondet druida =   CRIATURAS.filter(criatura => criatura.tipo_criatura == Druida).oneOf()
        nondet monstro =  CRIATURAS.filter(criatura => criatura.tipo_criatura == Monstro).oneOf()

        nondet personagem_inicial = CRIATURAS.filter(criatura => criatura.tipo_criatura != Monstro).oneOf()
        nondet monstro_inicial = CRIATURAS.filter(criatura => criatura.tipo_criatura == Monstro).oneOf()
        
        nondet dado_mago = DADO.oneOf()
        nondet dado_cacador = DADO.setRemove(dado_mago).oneOf()
        nondet dado_druida = DADO.setRemove(dado_mago).setRemove(dado_cacador).oneOf()
        nondet dado_monstro = DADO.setRemove(dado_mago).setRemove(dado_cacador).setRemove(dado_druida).oneOf()

        val crias = Set({...mago, numero_dado: dado_mago}, {...cacador, numero_dado: dado_cacador}, {...druida, numero_dado: dado_druida}, {...monstro, numero_dado: dado_monstro})

        val dado_e_criatura = crias.map(cria => (cria.numero_dado, cria)).setToMap()  // mudar criaturas
        val acabou_ataque = crias.fold(personagem_inicial, (acc, criatura) => if (criatura.numero_dado > acc.numero_dado) criatura else acc)
        val alvoo_ataque = if(acabou_ataque.tipo_criatura == Monstro) personagem_inicial else monstro_inicial
        all {
            monstrengo' = {...monstro, numero_dado: dado_monstro},
            idx_monstro'= dado_monstro,
            dado_criaturas' = dado_e_criatura,
            CRIATURAS' = dado_e_criatura.values(),
        }
    }



    // // Ação: Realiza um passo no combate
    action step: bool = {

        val crias_att = 
            if(CRIATURAS.exists(c => c.numero_dado > 0)){
                CRIATURAS
            }
            else {
                CRIATURAS.map(c => inverter_dado(c))
            }

        pure val personagem_inicial = CRIATURAS.fold(OVELHA, (acc, criatura) => if (criatura.numero_dado < acc.numero_dado and criatura.tipo_criatura != Monstro) criatura else acc)
        // nondet druida = crias_att.filter(criatura => criatura.tipo_criatura == Druida).oneOf()

        val criaturas_att_map = crias_att.map(c => abs(c.numero_dado) -> c).setToMap()
        val vai_atacar = dado_criaturas.values().fold(OVELHA, (acc, criatura) => if (criatura.numero_dado > acc.numero_dado) criatura else acc)
        val alvo_ataque = if(vai_atacar.tipo_criatura == Monstro) personagem_inicial else monstrengo
        all {
            // druida_backup_pra_quando_acabar_a_transformacao' = druida,
            not(empty(dado_criaturas.values())),
            not(empty(criaturas_att_map.values())),
            // pedir para a prof se as verificacoes em batalha ja sao o suficiente
            batalha(vai_atacar, alvo_ataque, idx_monstro, dado_criaturas),
            // dado_criaturas' = atualizar_atributos_das_criaturas(criaturas_att_map, acabou_de_atacar, criaturas_att_map.get(idx_monstro)),
            CRIATURAS' = dado_criaturas.values()
        }
    }

    val o_monstro_nao_morre = dado_criaturas.values().filter(c => c.tipo_criatura == Monstro).forall(c => c.hp > 0)
    val os_personagens_nao_morrem = dado_criaturas.values().filter(c => c.tipo_criatura != Monstro).forall(c => c.hp > 0)



//-----------------------------------------------------INVARIANTES--------------------------------------------------------------------

    // Invariante: O monstro e a criatura ainda estão vivos
    // val inv = crias.values().exists(p => p.hp > 0) and monstro.hp > 0

}




module jogo {
    import basicSpells.* from "basicSpells"
    // import rareSpells.* from "rareSpell"

    type TipoCriatura = Mago | Cacador | Druida | Monstro

    type Criatura = { 
        name: str,
        hp: int,
        dmg: int,
        numero_dado: int,
        ja_jogou: bool,
        tipo_criatura: TipoCriatura
    }

    pure val DADO = 1.to(20)
    
    pure val CRIATURAS = Set(
        { name: "Mago1", hp: 20, dmg: 10, ja_jogou: false,tipo_criatura: Mago },
        { name: "Cacador1", hp: 20, dmg: 10, ja_jogou: false, tipo_criatura: Cacador },
        { name: "Druida1", hp: 20, dmg: 10, ja_jogou: false, tipo_criatura: Druida },
        { name: "Mike wazousky", hp: 100, dmg: 10, ja_jogou: false, tipo_criatura: Monstro }
    )

    var criaturas: int -> Criatura
    var dado: int

    pure def dano_criatura(p: Criatura, d: int): Criatura = {
        { ...p, hp: p.hp - d }
    }

    pure def update_dmg_monstro(m: Criatura): Criatura = {
        { ...m, dmg: 20}
    }

    // Função para rolar o dado
    pure def rola_dado(): int = {
        DADO.oneOf()
    }

    pure def terminou_acao(c: Criatura): Criatura = {
        { ...c, ja_jogou: true}
    }

    // Ação: Monstro ataca o Criatura
    action ataque(attacker: Criatura, receiver: Criatura): bool = all {
        criaturas' = criaturas.setBy(receiver.name, p => p.dano_criatura(attacker.dmg))
    }


    // Ação: Ataque que decide o próximo movimento de acordo com quem ainda está vivo
    action batalha(attacker: Criatura, receiver: Criatura): bool = {
        all {
            attacker != receiver,
            attacker.hp > 0, 
            receiver.hp > 0,
            ataque(attacker, receiver),
            terminou_acao(attacker)
        }            
        
    }


    pure def map_dos_dados_rolados(): int -> Criatura = {
        CRIATURAS.map(c => rola_dado() -> c)
    }


    // Ação: Inicializa o jogo
    action init = {
        nondet criatura_inicial = CRIATURAS.oneOf()
        nondet monstro_inicial = CRIATURAS.oneOf()
        all {
            criaturas' = Map(
                criatura_inicial.name -> criatura_inicial
            )
        }
    }

    // Ação: Realiza um passo no combate
    action step = {
        // todos rolam os dados -> quem tirar o maior numero ataca primeiro -> escolher ataque -> atacar
        val fila = map_dos_dados_rolados()
        val atacante = CRIATURAS.filter(c => c.tipo_criatura != Monstro).values().oneOf()
        val alvo = CRIATURAS.filter(c => c.tipo_criatura != Monstro).values().oneOf()
        batalha(atacante, alvo)
    }


    // Invariante: O monstro ou o Criatura ainda estão vivos
    val inv = criaturas.values().exists(p => p.hp > 0) and monstro.hp > 0
}

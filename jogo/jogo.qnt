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
    var chave_max: int
    var atacante: Criatura

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

    pure def map_dos_dados_rolados(dado: int): int -> Criatura = {
        CRIATURAS.map(c => dado -> c)
    }

    action init = {
        // nondet criatura_inicial = CRIATURAS.filter(criatura => criatura.tipo_criatura != Monstro).oneOf()
        // nondet monstro_inicial = CRIATURAS.filter(criatura => criatura.tipo_criatura == Monstro).oneOf()
        nondet dado_1 = DADO.oneOf()
        nondet dado_2 = DADO.oneOf()
        nondet dado_3 = DADO.oneOf()
        nondet dado_4 = DADO.oneOf()
        nondet criatura_map = CRIATURAS.setOfMaps(Set(dado_1, dado_2, dado_3, dado_4)).oneOf()
        all {
            criaturas' = criatura_map, // criatura -> dado
            chave_max' = criaturas.fold(0, (acc, criatura) => if (criatura > acc) criatura else acc),
            atacante' = criaturas.get(chave_max),
        }
    }

    // Ação: Realiza um passo no combate
    action step = {
        // todos rolam os dados -> quem tirar o maior numero ataca primeiro -> escolher ataque -> atacar
        nondet dado = DADO.oneOf()
        def fila = map_dos_dados_rolados(dado)
        nondet atacante = CRIATURAS.filter(c => c.tipo_criatura != Monstro).values().oneOf()
        nondet alvo = CRIATURAS.filter(c => c.tipo_criatura != Monstro).values().oneOf()
        batalha(atacante, alvo)
    }


    // Invariante: O monstro ou o Criatura ainda estão vivos
    // val inv = criaturas.values().exists(p => p.hp > 0) and monstro.hp > 0
}

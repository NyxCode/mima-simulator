use mima_simulator::mima;

fn main() {
    let mut m = mima! {
                   * = 0x80;
                  i: static = 0;
                  j: static = 0;
                len: static = 0;
            min_idx: static = 0;
                tmp: static = 0;
                   * = 0x100;
        
        START:      LDC 1;
                    ADD 0x040;
                    STV len;
        outer:      LDV i;
                    EQL len;
                    JMN end;
                    LDV i;
                    STV min_idx;
                    LDC 1;
                    ADD i;
                    STV j;
        inner:      LDV j;
                    EQL len;
                    JMN swap;
                    LDIV j;
                    NOT;
                    STV tmp;
                    LDIV min_idx;
                    ADD tmp;
                    JMN inc_inner;
                    LDV j;
                    STV min_idx;
        inc_inner:  LDC 1;
                    ADD j;
                    STV j;
                    JMP inner;
        swap:       LDIV min_idx;
                    STV tmp;
                    LDIV i;
                    STIV min_idx;
                    LDV tmp;
                    STIV i;
                    LDC 1;
                    ADD i;
                    STV i;
                    JMP outer;
        end:        HALT;
    };

    m.memory[0x00] = 5;
    m.memory[0x01] = 12;
    m.memory[0x02] = 2;
    m.memory[0x03] = 6;
    m.memory[0x40] = 0x03;

    m.run();

    println!("{:#07X}", m.memory[0x00]);
    println!("{:#07X}", m.memory[0x01]);
    println!("{:#07X}", m.memory[0x02]);
    println!("{:#07X}", m.memory[0x03]);
}
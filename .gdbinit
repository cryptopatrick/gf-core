define print-rust-string
  printf "%s\n", $arg0.vec.buf.ptr.pointer.pointer
end

define cell-tag
  print $arg0.get_tag()
end

define cell-value
  print $arg0.get_value()
end

define index-cell
  print *($arg0.buf.ptr.pointer.pointer + $arg1)
end

define print-scryer-atom
  set $atom_table = scryer_prolog::atom_table::global_atom_table::GLOBAL_ATOM_TABLE
  set $inner_tbl  = $atom_table.value.value.value.value.data.value.ptr.pointer.data.inner.active_value.p.value
  set $block_base = $inner_tbl.block.base
  # 16 bytes is the size of &str on 64-bit machines, which are fat pointers
  set $STRINGS_len = sizeof(scryer_prolog::atom_table::STRINGS) / 16
  set $atom_idx = $arg0.index >> 3

  if $atom_idx < $STRINGS_len
    print scryer_prolog::atom_table::STRINGS[$atom_idx]
  else
    printf "%s\n", $block_base + ($atom_idx - $STRINGS_len) * 8 + 8
  end
end

define cell-to-atom-and-arity
  set $atom_cell = $arg0 as scryer_prolog::atom_table::AtomCell
  set $atom_and_arity = $atom_cell.get_name_and_arity()
  set $atom = $atom_and_arity.0
  print-scryer-atom $atom
  print $atom_and_arity.1
end
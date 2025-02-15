
#scope_module

Tracking_Allocator :: struct {
    allocator: Allocator;
    current_allocated: int;
    high_water_mark: int;
}

tracking_allocator_proc :: (mode: Allocator_Mode, size: s64, old_size: s64, old_memory: *void, allocator_data: *void) -> *void {
    ta := allocator_data.(*Tracking_Allocator);

    if mode == {
        case .ALLOCATE;
            ta.current_allocated += size;
        case .RESIZE;
            ta.current_allocated += size - old_size;
        case .FREE;
            if old_memory then ta.current_allocated -= old_memory.(*int)[-1];
    }

    ta.high_water_mark = max(ta.high_water_mark, ta.current_allocated);

    real_old_memory := old_memory;
    if real_old_memory then real_old_memory -= size_of(int);

    // Store size so we can adjust on free
    data := ta.allocator.proc(mode, size + size_of(int), old_size + size_of(int), real_old_memory, ta.allocator.data);
    if data then data.(*int).* = size;
    return data + size_of(int);
}


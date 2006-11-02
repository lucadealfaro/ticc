mdd_t* mdd_max_on_mdd(mdd_manager* mgr, mdd_t* mdd, int id);
mdd_t* mdd_max_on_bits(mdd_manager* mgr, mdd_t* mdd, int id);

#define mdd_max mdd_max_on_bits

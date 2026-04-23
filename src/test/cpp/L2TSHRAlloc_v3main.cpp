#include <iostream>
#include <iomanip>
#include <sstream>
#include <chrono>
#include <string>
#include <random>
#include <set>

#include <verilated_vcd_c.h>

#include "../../../verilated/VTestTop_L2TSHRAlloc.h"

#include "L2TSHRAlloc_conn.hpp"


// REQ, EVT, SNP
// 
// - PA hits (reuse)
//   - TSHR RBE free => reuse
//   - TSHR RBE busy => stall
//
// - PA miss (alloc)
//   - has free slots
//     - not reserved => alloc
//     - reserved for others => stall
//   - no free slots => stall
//
// - Idle

using VTestTop = VTestTop_L2TSHRAlloc;

static constexpr uint8_t EvictBack = 0x1E;

static VTestTop_L2TSHRAlloc* top = new VTestTop_L2TSHRAlloc;

static bool waveEnable = true;
static VerilatedVcdC* vcd = nullptr;

uint64_t t = 0;

inline static std::string GetWaveFileName()
{
    auto t = std::chrono::system_clock::to_time_t(std::chrono::system_clock::now());

    std::ostringstream oss;
    oss << "wave-";
    oss << std::put_time(std::localtime(&t), "%Y%m%d-%H-%M-%S");
    oss << ".vcd";
    return oss.str();
}

inline static bool IsInWaveTime(uint64_t time)
{
    return true;
}

inline static void V3Reset(uint64_t& time, VTestTop* top, uint64_t n)
{
    for (uint64_t i = 0; i < n; i++)
    {
        top->reset = 1;

        top->clock = 0;
        top->eval(); 
        if (waveEnable && IsInWaveTime(time))
            vcd->dump(time);
        time++;

        top->clock = 1;
        top->eval(); 
        if (waveEnable && IsInWaveTime(time))
            vcd->dump(time);
        time++;
    }

    top->reset = 0;
}

inline static void V3EvalNegedge(uint64_t& time, VTestTop* top)
{
    top->clock = 0;
    top->eval();

    if (waveEnable && IsInWaveTime(time))
        vcd->dump(time);

    time++;
}

inline static void V3EvalPosedge(uint64_t& time, VTestTop* top)
{
    top->clock = 1;
    top->eval();

    if (waveEnable && IsInWaveTime(time))
        vcd->dump(time);

    time++;
}

void V3PullLogPerf(VTestTop* top, uint8_t dump, uint8_t clean)
{
    top->log_dump   = dump;
    top->log_clean  = clean;
}


int IsPAHit(uint64_t paddr, const L2TSHRAlloc::BundleFromTSHR& fromTSHR)
{
    for (size_t i = 0; i < L2TSHRAlloc::TSHR_NUM; i++)
    {
        if (*fromTSHR.valid[i] && (*fromTSHR.paddr[i] >> 6) == paddr)
            return (i + 1);
    }
    return 0;
}


#define error(msg) \
    { \
        std::cerr << "[" << t << "] %Error: " << msg << std::endl; \
        error++; \
    }

#define error_if(cond, msg) \
    if (cond) { error(msg) } else { }

int main()
{
    size_t timeLimit = 10000000;
    size_t seed = 0;

    size_t error = 0;

    size_t idx_resv_L2EVT = L2TSHRAlloc::TSHR_NUM - 1;
    size_t idx_resv_L1EVT = L2TSHRAlloc::TSHR_NUM - 2;
    size_t idx_resv_L3SNP = L2TSHRAlloc::TSHR_NUM - 3;

    size_t tc_PA_hits_reuse_EVT = 0;
    size_t tc_PA_hits_reuse_SNP = 0;
    size_t tc_PA_hits_reuse_REQ = 0;
    size_t tc_PA_hits_stall_EVT = 0;
    size_t tc_PA_hits_stall_SNP = 0;
    size_t tc_PA_hits_stall_REQ = 0;
    size_t tc_PA_miss_alloc_EVT = 0;
    size_t tc_PA_miss_alloc_SNP = 0;
    size_t tc_PA_miss_alloc_REQ = 0;
    size_t tc_PA_miss_alloc_resv_L1EVT = 0;
    size_t tc_PA_miss_alloc_resv_L2EVT = 0;
    size_t tc_PA_miss_alloc_resv_L3SNP = 0;
    size_t tc_PA_miss_stall_EVT = 0;
    size_t tc_PA_miss_stall_SNP = 0;
    size_t tc_PA_miss_stall_REQ = 0;

    size_t pc_parallelism_1 = 0;
    size_t pc_parallelism_2 = 0;
    size_t pc_parallelism_3 = 0;

    std::mt19937_64 rand64(seed);

    if (waveEnable)
    {
        Verilated::traceEverOn(true);
        vcd = new VerilatedVcdC;
        top->trace(vcd, 99);
        vcd->open(GetWaveFileName().c_str());
    }

    L2TSHRAlloc::BundleFromTSHR     fromTSHR        (top);
    L2TSHRAlloc::BundleToTSHR       toTSHR          (top);
    L2TSHRAlloc::BundleFromTSHRCtrl fromTSHRCtrl    (top);

    V3PullLogPerf(top, 0, 0);

    V3Reset(t, top, 10);

    int tmp;

    while (t < timeLimit)
    {
        // check ToTSHR
        size_t parallelism = 0;
        for (size_t i = 0; i < L2TSHRAlloc::TSHR_NUM; i++)
        {
            if (*toTSHR.allocEVT[i] || *toTSHR.allocSNP[i] || *toTSHR.allocREQ[i]
             || *toTSHR.reuseEVT[i] || *toTSHR.reuseSNP[i] || *toTSHR.reuseREQ[i])
            {
                if (*toTSHR.allocEVT[i])
                {
                    parallelism++;

                    // check input
                    if (*fromTSHRCtrl.RXEVT_ready && *fromTSHRCtrl.RXEVT_valid)
                        error_if((*toTSHR.paddr[i] >> 6) != (*fromTSHRCtrl.RXEVT_bits_Addr >> 6),
                            "Addr mismatch on EVT " << i << " alloc")
                    else
                        error("EVT " << i << " alloc without fired RXEVT input");

                    // check PA miss
                    error_if((tmp = IsPAHit(*toTSHR.paddr[i] >> 6, fromTSHR)),
                        "PA hit TSHR " << (tmp - 1) << " on EVT " << i << " alloc");

                    // check reservation
                    error_if(i == idx_resv_L2EVT, "reserved slot " << i << " for L2EVT is allocated to EVT");
                    error_if(i == idx_resv_L3SNP, "reserved slot " << i << " for L3SNP is allocated to EVT");

                    // collect TCs
                    bool is_alloc_resv = i == idx_resv_L1EVT;
                    for (size_t j = 0; j < L2TSHRAlloc::TSHR_NUM && is_alloc_resv; j++)
                    {
                        if (j == idx_resv_L1EVT) continue;
                        if (j == idx_resv_L2EVT) continue;
                        if (j == idx_resv_L3SNP) continue;
                        if (!*fromTSHR.valid[j]) is_alloc_resv = false;
                    }
                    if (is_alloc_resv)
                        tc_PA_miss_alloc_resv_L1EVT++;

                    tc_PA_miss_alloc_EVT++;
                }

                if (*toTSHR.allocSNP[i])
                {
                    parallelism++;

                    // check input
                    if (*fromTSHRCtrl.RXSNP_ready && *fromTSHRCtrl.RXSNP_valid)
                        error_if((*toTSHR.paddr[i] >> 6) != (*fromTSHRCtrl.RXSNP_bits_Addr >> 3),
                            "Addr mismatch on SNP " << i << " alloc")
                    else
                        error("SNP " << i << " alloc without fired RXSNP input");

                    // check PA miss
                    error_if((tmp = IsPAHit(*toTSHR.paddr[i] >> 6, fromTSHR)),
                        "PA hit TSHR " << (tmp - 1) << " on SNP " << i << " alloc");

                    // check reservation
                    error_if(i == idx_resv_L1EVT, "reserved slot " << i << " for L1EVT is allocated to SNP");
                    error_if(i == idx_resv_L2EVT, "reserved slot " << i << " for L2EVT is allocated to SNP");

                    // collect TCs
                    bool is_alloc_resv = i == idx_resv_L3SNP;
                    for (size_t j = 0; j < L2TSHRAlloc::TSHR_NUM && is_alloc_resv; j++)
                    {
                        if (j == idx_resv_L1EVT) continue;
                        if (j == idx_resv_L2EVT) continue;
                        if (j == idx_resv_L3SNP) continue;
                        if (!*fromTSHR.valid[j]) is_alloc_resv = false;
                    }
                    if (is_alloc_resv)
                        tc_PA_miss_alloc_resv_L3SNP++;

                    tc_PA_miss_alloc_SNP++;
                }

                if (*toTSHR.allocREQ[i])
                {
                    parallelism++;

                    // check input
                    if (*fromTSHRCtrl.RXREQ_ready && *fromTSHRCtrl.RXREQ_valid)
                        error_if((*toTSHR.paddr[i] >> 6) != (*fromTSHRCtrl.RXREQ_bits_Addr >> 6),
                            "Addr mismatch on REQ " << i << " alloc")
                    else
                        error("REQ " << i << " alloc without fired RXREQ input");

                    // check PA miss
                    error_if((tmp = IsPAHit(*toTSHR.paddr[i] >> 6, fromTSHR)),
                        "PA hit TSHR " << (tmp - 1) << " on REQ " << i << " alloc");

                    // check reservation
                    error_if(i == idx_resv_L1EVT, "reserved slot " << i << " for L1EVT is allocated to REQ");
                    error_if(i == idx_resv_L2EVT && *fromTSHRCtrl.RXREQ_bits_Opcode != EvictBack,
                        "reserved slot " << i << " for L2EVT is allocated to REQ with non-EvictBack opcode");
                    error_if(i == idx_resv_L3SNP, "reserved slot " << i << " for L3SNP is allocated to REQ");

                    // collect TCs
                    bool is_alloc_resv = i == idx_resv_L2EVT;
                    for (size_t j = 0; j < L2TSHRAlloc::TSHR_NUM && is_alloc_resv; j++)
                    {
                        if (j == idx_resv_L1EVT) continue;
                        if (j == idx_resv_L2EVT) continue;
                        if (j == idx_resv_L3SNP) continue;
                        if (!*fromTSHR.valid[j]) is_alloc_resv = false;
                    }
                    if (is_alloc_resv)
                        tc_PA_miss_alloc_resv_L2EVT++;

                    tc_PA_miss_alloc_REQ++;
                }

                if (*toTSHR.reuseEVT[i])
                {
                    parallelism++;

                    // check input
                    if (*fromTSHRCtrl.RXEVT_ready && *fromTSHRCtrl.RXEVT_valid)
                        error_if((*fromTSHR.paddr[i] >> 6) != (*fromTSHRCtrl.RXEVT_bits_Addr >> 6),
                            "Addr mismatch on EVT " << i << " reuse")
                    else
                        error("EVT " << i << " reuse without fired RXEVT input");

                    // check PA hit
                    // NOTICE: ToTSHR.paddr not valid on use
                    /*
                    error_if((*toTSHR.paddr[i] >> 6) != (*fromTSHR.paddr[i] >> 6),
                        "PA miss on EVT " << i << " reuse");
                    */

                    // check RBE state
                    error_if(*fromTSHR.busyEVT[i], "RBE busy on EVT " << i << " reuse");

                    // collect TCs
                    tc_PA_hits_reuse_EVT++;
                }

                if (*toTSHR.reuseSNP[i])
                {
                    parallelism++;

                    // check input
                    if (*fromTSHRCtrl.RXSNP_ready && *fromTSHRCtrl.RXSNP_valid)
                        error_if((*fromTSHR.paddr[i] >> 6) != (*fromTSHRCtrl.RXSNP_bits_Addr >> 3),
                            "Addr mismatch on SNP " << i << " reuse")
                    else
                        error("SNP " << i << " reuse without fired RXSNP input");

                    // check PA hit
                    // NOTICE: ToTSHR.paddr not valid on use
                    /*
                    error_if((*toTSHR.paddr[i] >> 3) != (*fromTSHR.paddr[i] >> 6),
                        "PA miss on SNP " << i << " reuse");
                    */

                    // check RBE state
                    error_if(*fromTSHR.busySNP[i], "RBE busy on SNP " << i << " reuse");

                    // collect TCs
                    tc_PA_hits_reuse_SNP++;
                }

                if (*toTSHR.reuseREQ[i])
                {
                    parallelism++;

                    // check input 
                    if (*fromTSHRCtrl.RXREQ_ready && *fromTSHRCtrl.RXREQ_valid)
                        error_if((*fromTSHR.paddr[i] >> 6) != (*fromTSHRCtrl.RXREQ_bits_Addr >> 6),
                            "Addr mismatch on REQ " << i << " reuse")
                    else
                        error("REQ " << i << " reuse without fired RXREQ input");

                    // check PA hit
                    // NOTICE: ToTSHR.paddr not valid on use
                    /*
                    error_if((*toTSHR.paddr[i] >> 6) != (*fromTSHR.paddr[i] >> 6),
                        "PA miss on REQ " << i << " reuse");
                    */

                    // check RBE state
                    error_if(*fromTSHR.busyREQ[i], "RBE busy on REQ " << i << " reuse");

                    // collect TCs
                    tc_PA_hits_reuse_REQ++;
                }

                error_if(parallelism > 3, "too much parallelism on ToTSHR");
            }
        }

        // check PA uniqueness of FromTSHR
        std::set<uint64_t> fromTSHR_PA_set;
        for (int i = 0; i < L2TSHRAlloc::TSHR_NUM; i++)
        {
            if (*fromTSHR.valid[i])
            {
                uint64_t pa = *fromTSHR.paddr[i] >> 6;
                error_if(fromTSHR_PA_set.count(pa), "PA not unique in FromTSHR");
                fromTSHR_PA_set.insert(pa);
            }
        }

        // check PA uniqueness of alloc/reuse
        std::set<uint64_t> toTSHR_PA_set;
        for (int i = 0; i < L2TSHRAlloc::TSHR_NUM; i++)
        {
            if (*toTSHR.allocEVT[i] || *toTSHR.allocSNP[i] || *toTSHR.allocREQ[i])
            {
                uint64_t pa = *toTSHR.paddr[i] >> 6;
                error_if(toTSHR_PA_set.count(pa), "PA not unique in alloc/reuse");
                toTSHR_PA_set.insert(pa);
            }
            else if (*toTSHR.reuseEVT[i] || *toTSHR.reuseSNP[i] || *toTSHR.reuseREQ[i])
            {
                uint64_t pa = *fromTSHR.paddr[i] >> 6;
                error_if(toTSHR_PA_set.count(pa), "PA not unique in alloc/reuse");
                toTSHR_PA_set.insert(pa);
            }
        }

        // check alloc/reuse exclusiveness
        for (int i = 0; i < L2TSHRAlloc::TSHR_NUM; i++)
        {
            if (*toTSHR.allocEVT[i] || *toTSHR.allocSNP[i] || *toTSHR.allocREQ[i])
            {
                if (*toTSHR.reuseEVT[i] || *toTSHR.reuseSNP[i] || *toTSHR.reuseREQ[i])
                    error("alloc and reuse both valid on entry " << i);
            }
        }

        // check no alloc on valid
        for (int i = 0; i < L2TSHRAlloc::TSHR_NUM; i++)
        {
            if (*fromTSHR.valid[i])
            {
                error_if(*toTSHR.allocEVT[i] || *toTSHR.allocSNP[i] || *toTSHR.allocREQ[i],
                    "alloc on valid entry " << i);
            }
        }

        // check no reuse on invalid
        for (int i = 0; i < L2TSHRAlloc::TSHR_NUM; i++)
        {
            if (!*fromTSHR.valid[i])
            {
                error_if(*toTSHR.reuseEVT[i] || *toTSHR.reuseSNP[i] || *toTSHR.reuseREQ[i],
                    "reuse on invalid entry " << i);
            }
        }

        // check fired input related output
        if (*fromTSHRCtrl.RXEVT_ready && *fromTSHRCtrl.RXEVT_valid)
        {
            bool present = false;
            for (int i = 0; i < L2TSHRAlloc::TSHR_NUM; i++)
                if (*toTSHR.allocEVT[i])
                {
                    if (*toTSHR.paddr[i] == *fromTSHRCtrl.RXEVT_bits_Addr)
                        present = true;
                }
                else if (*toTSHR.reuseEVT[i])
                {
                    if (*fromTSHR.paddr[i] == *fromTSHRCtrl.RXEVT_bits_Addr)
                        present = true;
                }

            error_if(!present, "no alloc/reuse output for fired RXEVT input");
        }

        if (*fromTSHRCtrl.RXSNP_ready && *fromTSHRCtrl.RXSNP_valid)
        {
            bool present = false;
            for (int i = 0; i < L2TSHRAlloc::TSHR_NUM; i++)
                if (*toTSHR.allocSNP[i])
                {
                    if (*toTSHR.paddr[i] == (*fromTSHRCtrl.RXSNP_bits_Addr << 3))
                        present = true;
                }
                else if (*toTSHR.reuseSNP[i])
                {
                    if (*fromTSHR.paddr[i] == (*fromTSHRCtrl.RXSNP_bits_Addr << 3))
                        present = true;
                }

            error_if(!present, "no alloc/reuse output for fired RXSNP input");
        }

        if (*fromTSHRCtrl.RXREQ_ready && *fromTSHRCtrl.RXREQ_valid)
        {
            bool present = false;
            for (int i = 0; i < L2TSHRAlloc::TSHR_NUM; i++)
                if (*toTSHR.allocREQ[i])
                {
                    if (*toTSHR.paddr[i] == *fromTSHRCtrl.RXREQ_bits_Addr)
                        present = true;
                }
                else if (*toTSHR.reuseREQ[i])
                {
                    if (*fromTSHR.paddr[i] == *fromTSHRCtrl.RXREQ_bits_Addr)
                        present = true;
                }

            error_if(!present, "no alloc/reuse output for fired RXREQ input");
        }

        // collect stall TCs
        if (!*fromTSHRCtrl.RXEVT_ready && *fromTSHRCtrl.RXEVT_valid)
        {
            if (IsPAHit(*fromTSHRCtrl.RXEVT_bits_Opcode >> 6, fromTSHR))
                tc_PA_hits_stall_EVT++;
            else
                tc_PA_miss_stall_EVT++;
        }

        if (!*fromTSHRCtrl.RXSNP_ready && *fromTSHRCtrl.RXSNP_valid)
        {
            if (IsPAHit(*fromTSHRCtrl.RXSNP_bits_Opcode >> 3, fromTSHR))
                tc_PA_hits_stall_SNP++;
            else
                tc_PA_miss_stall_SNP++;
        }

        if (!*fromTSHRCtrl.RXREQ_ready && *fromTSHRCtrl.RXREQ_valid)
        {
            if (IsPAHit(*fromTSHRCtrl.RXREQ_bits_Opcode >> 6, fromTSHR))
                tc_PA_hits_stall_REQ++;
            else
                tc_PA_miss_stall_REQ++;
        }

        // collect PCs
        if (parallelism == 1) pc_parallelism_1++;
        else if (parallelism == 2) pc_parallelism_2++;
        else if (parallelism == 3) pc_parallelism_3++;

        // TODO: replace with better test cases
        // Full Random Fuzz
        if (!error)
        {
            *fromTSHRCtrl.RXEVT_valid = rand64() & 0x1;
            *fromTSHRCtrl.RXEVT_bits_Opcode = (rand64() & 0x3F);
            *fromTSHRCtrl.RXSNP_valid = rand64() & 0x1;
            *fromTSHRCtrl.RXSNP_bits_Opcode = (rand64() & 0x1F);
            *fromTSHRCtrl.RXREQ_valid = rand64() & 0x1;
            *fromTSHRCtrl.RXREQ_bits_Opcode = (rand64() & 0x3F);

            do {
                *fromTSHRCtrl.RXEVT_bits_Addr = (rand64() & 0x1F) << 6;
            } while (false);
            
            do {
                *fromTSHRCtrl.RXSNP_bits_Addr = (rand64() & 0x1F) << 3;
            } while ((*fromTSHRCtrl.RXSNP_bits_Addr >> 3) == (*fromTSHRCtrl.RXEVT_bits_Addr >> 6));
           
            do {
                *fromTSHRCtrl.RXREQ_bits_Addr = (rand64() & 0x1F) << 6;
            } while ((*fromTSHRCtrl.RXREQ_bits_Addr >> 6) == (*fromTSHRCtrl.RXEVT_bits_Addr >> 6)
                  || (*fromTSHRCtrl.RXREQ_bits_Addr >> 6) == (*fromTSHRCtrl.RXSNP_bits_Addr >> 3));

            for (int i = 0; i < L2TSHRAlloc::TSHR_NUM; i++)
                *fromTSHR.paddr[i] = 0;

            for (int i = 0; i < L2TSHRAlloc::TSHR_NUM; i++)
            {
                *fromTSHR.valid[i] = rand64() & 0x1;
                *fromTSHR.busyEVT[i] = rand64() & 0x1;
                *fromTSHR.busySNP[i] = rand64() & 0x1;
                *fromTSHR.busyREQ[i] = rand64() & 0x1;

                bool collision = true;
                while (*fromTSHR.valid[i] && collision) 
                {
                    collision = false;

                    *fromTSHR.paddr[i] = (rand64() & 0x1F) << 6;

                    for (int j = 0; j < i; j++)
                        if (*fromTSHR.valid[j] && (*fromTSHR.paddr[j] >> 6) == (*fromTSHR.paddr[i] >> 6))
                            collision = true;
                }
            }
        }
        //

        V3EvalNegedge(t, top);
        V3EvalPosedge(t, top);

        if (error)
            break;
    }

    // Print summary
    std::cout << "Test Summary:" << std::endl;
    std::cout << "tc_PA_hits_reuse_EVT: " << tc_PA_hits_reuse_EVT << std::endl;
    std::cout << "tc_PA_hits_reuse_SNP: " << tc_PA_hits_reuse_SNP << std::endl;
    std::cout << "tc_PA_hits_reuse_REQ: " << tc_PA_hits_reuse_REQ << std::endl;
    std::cout << "tc_PA_hits_stall_EVT: " << tc_PA_hits_stall_EVT << std::endl;
    std::cout << "tc_PA_hits_stall_SNP: " << tc_PA_hits_stall_SNP << std::endl;
    std::cout << "tc_PA_hits_stall_REQ: " << tc_PA_hits_stall_REQ << std::endl;
    std::cout << "tc_PA_miss_alloc_EVT: " << tc_PA_miss_alloc_EVT << std::endl;
    std::cout << "tc_PA_miss_alloc_SNP: " << tc_PA_miss_alloc_SNP << std::endl;
    std::cout << "tc_PA_miss_alloc_REQ: " << tc_PA_miss_alloc_REQ << std::endl;
    std::cout << "tc_PA_miss_alloc_resv_L1EVT: " << tc_PA_miss_alloc_resv_L1EVT << std::endl;
    std::cout << "tc_PA_miss_alloc_resv_L2EVT: " << tc_PA_miss_alloc_resv_L2EVT << std::endl;
    std::cout << "tc_PA_miss_alloc_resv_L3SNP: " << tc_PA_miss_alloc_resv_L3SNP << std::endl;
    std::cout << "tc_PA_miss_stall_EVT: " << tc_PA_miss_stall_EVT << std::endl;
    std::cout << "tc_PA_miss_stall_SNP: " << tc_PA_miss_stall_SNP << std::endl;
    std::cout << "tc_PA_miss_stall_REQ: " << tc_PA_miss_stall_REQ << std::endl;
    std::cout << "pc_parallelism_1: " << pc_parallelism_1 << std::endl;
    std::cout << "pc_parallelism_2: " << pc_parallelism_2 << std::endl;
    std::cout << "pc_parallelism_3: " << pc_parallelism_3 << std::endl;

    if (waveEnable)
        vcd->close();

    return error;
}


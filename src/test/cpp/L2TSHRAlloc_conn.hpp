#pragma once

#include "../../../verilated/VTestTop_L2TSHRAlloc.h"


namespace L2TSHRAlloc {

    inline constexpr size_t TSHR_NUM = 8;

    class BundleFromTSHR {
    public:
        uint8_t**     valid;
        uint64_t**    paddr;
        uint8_t**     busyEVT;
        uint8_t**     busySNP;
        uint8_t**     busyREQ;

    public:
        BundleFromTSHR(VTestTop_L2TSHRAlloc* top) noexcept;
        ~BundleFromTSHR() noexcept;
    };

    class BundleToTSHR {
    public:
        const uint64_t**  paddr;
        const uint8_t**   allocEVT;
        const uint8_t**   allocSNP;
        const uint8_t**   allocREQ;
        const uint8_t**   reuseEVT;
        const uint8_t**   reuseSNP;
        const uint8_t**   reuseREQ;

    public:
        BundleToTSHR(VTestTop_L2TSHRAlloc* top) noexcept;
        ~BundleToTSHR() noexcept;
    };

    class BundleFromTSHRCtrl {
    public:
        const uint8_t*  RXEVT_ready;
        uint8_t*        RXEVT_valid;
        uint8_t*        RXEVT_bits_Opcode;
        uint64_t*       RXEVT_bits_Addr;

        const uint8_t*  RXSNP_ready;
        uint8_t*        RXSNP_valid;
        uint8_t*        RXSNP_bits_Opcode;
        uint64_t*       RXSNP_bits_Addr;

        const uint8_t*  RXREQ_ready;
        uint8_t*        RXREQ_valid;
        uint8_t*        RXREQ_bits_Opcode;
        uint64_t*       RXREQ_bits_Addr;

    public:
        BundleFromTSHRCtrl(VTestTop_L2TSHRAlloc* top) noexcept;
        ~BundleFromTSHRCtrl() noexcept;
    };
}


// Implementation of: class BundleFromTSHR
namespace L2TSHRAlloc {
    
    inline BundleFromTSHR::BundleFromTSHR(VTestTop_L2TSHRAlloc* top) noexcept
        : valid     (new uint8_t*[TSHR_NUM])
        , paddr     (new uint64_t*[TSHR_NUM])
        , busyEVT   (new uint8_t*[TSHR_NUM])
        , busySNP   (new uint8_t*[TSHR_NUM])
        , busyREQ   (new uint8_t*[TSHR_NUM])
    {
        *(valid[0] = &top->io_fromTSHR_0_valid) = 0;
        *(valid[1] = &top->io_fromTSHR_1_valid) = 0;
        *(valid[2] = &top->io_fromTSHR_2_valid) = 0;
        *(valid[3] = &top->io_fromTSHR_3_valid) = 0;
        *(valid[4] = &top->io_fromTSHR_4_valid) = 0;
        *(valid[5] = &top->io_fromTSHR_5_valid) = 0;
        *(valid[6] = &top->io_fromTSHR_6_valid) = 0;
        *(valid[7] = &top->io_fromTSHR_7_valid) = 0;

        *(paddr[0] = &top->io_fromTSHR_0_bits_paddr) = 0;
        *(paddr[1] = &top->io_fromTSHR_1_bits_paddr) = 0;
        *(paddr[2] = &top->io_fromTSHR_2_bits_paddr) = 0;
        *(paddr[3] = &top->io_fromTSHR_3_bits_paddr) = 0;
        *(paddr[4] = &top->io_fromTSHR_4_bits_paddr) = 0;
        *(paddr[5] = &top->io_fromTSHR_5_bits_paddr) = 0;
        *(paddr[6] = &top->io_fromTSHR_6_bits_paddr) = 0;
        *(paddr[7] = &top->io_fromTSHR_7_bits_paddr) = 0;

        *(busyEVT[0] = &top->io_fromTSHR_0_bits_busy_EVT) = 0;
        *(busyEVT[1] = &top->io_fromTSHR_1_bits_busy_EVT) = 0;
        *(busyEVT[2] = &top->io_fromTSHR_2_bits_busy_EVT) = 0;
        *(busyEVT[3] = &top->io_fromTSHR_3_bits_busy_EVT) = 0;
        *(busyEVT[4] = &top->io_fromTSHR_4_bits_busy_EVT) = 0;
        *(busyEVT[5] = &top->io_fromTSHR_5_bits_busy_EVT) = 0;
        *(busyEVT[6] = &top->io_fromTSHR_6_bits_busy_EVT) = 0;
        *(busyEVT[7] = &top->io_fromTSHR_7_bits_busy_EVT) = 0;

        *(busySNP[0] = &top->io_fromTSHR_0_bits_busy_SNP) = 0;
        *(busySNP[1] = &top->io_fromTSHR_1_bits_busy_SNP) = 0;
        *(busySNP[2] = &top->io_fromTSHR_2_bits_busy_SNP) = 0;
        *(busySNP[3] = &top->io_fromTSHR_3_bits_busy_SNP) = 0;
        *(busySNP[4] = &top->io_fromTSHR_4_bits_busy_SNP) = 0;
        *(busySNP[5] = &top->io_fromTSHR_5_bits_busy_SNP) = 0;
        *(busySNP[6] = &top->io_fromTSHR_6_bits_busy_SNP) = 0;
        *(busySNP[7] = &top->io_fromTSHR_7_bits_busy_SNP) = 0;

        *(busyREQ[0] = &top->io_fromTSHR_0_bits_busy_REQ) = 0;
        *(busyREQ[1] = &top->io_fromTSHR_1_bits_busy_REQ) = 0;
        *(busyREQ[2] = &top->io_fromTSHR_2_bits_busy_REQ) = 0;
        *(busyREQ[3] = &top->io_fromTSHR_3_bits_busy_REQ) = 0;
        *(busyREQ[4] = &top->io_fromTSHR_4_bits_busy_REQ) = 0;
        *(busyREQ[5] = &top->io_fromTSHR_5_bits_busy_REQ) = 0;
        *(busyREQ[6] = &top->io_fromTSHR_6_bits_busy_REQ) = 0;
        *(busyREQ[7] = &top->io_fromTSHR_7_bits_busy_REQ) = 0;
    }

    inline BundleFromTSHR::~BundleFromTSHR() noexcept 
    {
        delete[] paddr;
        delete[] busyEVT;
        delete[] busySNP;
        delete[] busyREQ;
    }
}

// Implementation of: class BundleToTSHR
namespace L2TSHRAlloc {

    inline BundleToTSHR::BundleToTSHR(VTestTop_L2TSHRAlloc* top) noexcept
        : paddr     (new const uint64_t*[TSHR_NUM])
        , allocEVT  (new const uint8_t*[TSHR_NUM])
        , allocSNP  (new const uint8_t*[TSHR_NUM])
        , allocREQ  (new const uint8_t*[TSHR_NUM])
        , reuseEVT  (new const uint8_t*[TSHR_NUM])
        , reuseSNP  (new const uint8_t*[TSHR_NUM])
        , reuseREQ  (new const uint8_t*[TSHR_NUM])
    {
        paddr[0] = &top->io_toTSHR_0_paddr;
        paddr[1] = &top->io_toTSHR_1_paddr;
        paddr[2] = &top->io_toTSHR_2_paddr;
        paddr[3] = &top->io_toTSHR_3_paddr;
        paddr[4] = &top->io_toTSHR_4_paddr;
        paddr[5] = &top->io_toTSHR_5_paddr;
        paddr[6] = &top->io_toTSHR_6_paddr;
        paddr[7] = &top->io_toTSHR_7_paddr;

        allocEVT[0] = &top->io_toTSHR_0_alloc_EVT;
        allocEVT[1] = &top->io_toTSHR_1_alloc_EVT;
        allocEVT[2] = &top->io_toTSHR_2_alloc_EVT;
        allocEVT[3] = &top->io_toTSHR_3_alloc_EVT;
        allocEVT[4] = &top->io_toTSHR_4_alloc_EVT;
        allocEVT[5] = &top->io_toTSHR_5_alloc_EVT;
        allocEVT[6] = &top->io_toTSHR_6_alloc_EVT;
        allocEVT[7] = &top->io_toTSHR_7_alloc_EVT;

        allocSNP[0] = &top->io_toTSHR_0_alloc_SNP;
        allocSNP[1] = &top->io_toTSHR_1_alloc_SNP;
        allocSNP[2] = &top->io_toTSHR_2_alloc_SNP;
        allocSNP[3] = &top->io_toTSHR_3_alloc_SNP;
        allocSNP[4] = &top->io_toTSHR_4_alloc_SNP;
        allocSNP[5] = &top->io_toTSHR_5_alloc_SNP;
        allocSNP[6] = &top->io_toTSHR_6_alloc_SNP;
        allocSNP[7] = &top->io_toTSHR_7_alloc_SNP;

        allocREQ[0] = &top->io_toTSHR_0_alloc_REQ;
        allocREQ[1] = &top->io_toTSHR_1_alloc_REQ;
        allocREQ[2] = &top->io_toTSHR_2_alloc_REQ;
        allocREQ[3] = &top->io_toTSHR_3_alloc_REQ;
        allocREQ[4] = &top->io_toTSHR_4_alloc_REQ;
        allocREQ[5] = &top->io_toTSHR_5_alloc_REQ;
        allocREQ[6] = &top->io_toTSHR_6_alloc_REQ;
        allocREQ[7] = &top->io_toTSHR_7_alloc_REQ;

        reuseEVT[0] = &top->io_toTSHR_0_reuse_EVT;
        reuseEVT[1] = &top->io_toTSHR_1_reuse_EVT;
        reuseEVT[2] = &top->io_toTSHR_2_reuse_EVT;
        reuseEVT[3] = &top->io_toTSHR_3_reuse_EVT;
        reuseEVT[4] = &top->io_toTSHR_4_reuse_EVT;
        reuseEVT[5] = &top->io_toTSHR_5_reuse_EVT;
        reuseEVT[6] = &top->io_toTSHR_6_reuse_EVT;
        reuseEVT[7] = &top->io_toTSHR_7_reuse_EVT;

        reuseSNP[0] = &top->io_toTSHR_0_reuse_SNP;
        reuseSNP[1] = &top->io_toTSHR_1_reuse_SNP;
        reuseSNP[2] = &top->io_toTSHR_2_reuse_SNP;
        reuseSNP[3] = &top->io_toTSHR_3_reuse_SNP;
        reuseSNP[4] = &top->io_toTSHR_4_reuse_SNP;
        reuseSNP[5] = &top->io_toTSHR_5_reuse_SNP;
        reuseSNP[6] = &top->io_toTSHR_6_reuse_SNP;
        reuseSNP[7] = &top->io_toTSHR_7_reuse_SNP;

        reuseREQ[0] = &top->io_toTSHR_0_reuse_REQ;
        reuseREQ[1] = &top->io_toTSHR_1_reuse_REQ;
        reuseREQ[2] = &top->io_toTSHR_2_reuse_REQ;
        reuseREQ[3] = &top->io_toTSHR_3_reuse_REQ;
        reuseREQ[4] = &top->io_toTSHR_4_reuse_REQ;
        reuseREQ[5] = &top->io_toTSHR_5_reuse_REQ;
        reuseREQ[6] = &top->io_toTSHR_6_reuse_REQ;
        reuseREQ[7] = &top->io_toTSHR_7_reuse_REQ;
    }

    inline BundleToTSHR::~BundleToTSHR() noexcept 
    {
        delete[] paddr;
        delete[] allocEVT;
        delete[] allocSNP;
        delete[] allocREQ;
        delete[] reuseEVT;
        delete[] reuseSNP;
        delete[] reuseREQ;
    }
}

// Implementation of: class BundleFromTSHRCtrl
namespace L2TSHRAlloc {

    inline BundleFromTSHRCtrl::BundleFromTSHRCtrl(VTestTop_L2TSHRAlloc* top) noexcept
        : RXEVT_ready       (&top->io_fromTSHRCtrl_RXEVT_ready)
        , RXEVT_valid       (&top->io_fromTSHRCtrl_RXEVT_valid)
        , RXEVT_bits_Opcode (&top->io_fromTSHRCtrl_RXEVT_bits_Opcode)
        , RXEVT_bits_Addr   (&top->io_fromTSHRCtrl_RXEVT_bits_Addr)
        , RXSNP_ready       (&top->io_fromTSHRCtrl_RXSNP_ready)
        , RXSNP_valid       (&top->io_fromTSHRCtrl_RXSNP_valid)
        , RXSNP_bits_Opcode (&top->io_fromTSHRCtrl_RXSNP_bits_Opcode)
        , RXSNP_bits_Addr   (&top->io_fromTSHRCtrl_RXSNP_bits_Addr)
        , RXREQ_ready       (&top->io_fromTSHRCtrl_RXREQ_ready)
        , RXREQ_valid       (&top->io_fromTSHRCtrl_RXREQ_valid)
        , RXREQ_bits_Opcode (&top->io_fromTSHRCtrl_RXREQ_bits_Opcode)
        , RXREQ_bits_Addr   (&top->io_fromTSHRCtrl_RXREQ_bits_Addr)
    {
    //  top->io_fromTSHRCtrl_RXEVT_ready = 0;
        top->io_fromTSHRCtrl_RXEVT_valid = 0;
        top->io_fromTSHRCtrl_RXEVT_bits_TxnID = 0;
        top->io_fromTSHRCtrl_RXEVT_bits_Opcode = 0;
        top->io_fromTSHRCtrl_RXEVT_bits_NS = 0;
        top->io_fromTSHRCtrl_RXEVT_bits_WayValid = 0;
        top->io_fromTSHRCtrl_RXEVT_bits_Way = 0;
        top->io_fromTSHRCtrl_RXEVT_bits_TraceTag = 0;
        top->io_fromTSHRCtrl_RXEVT_bits_Addr = 0;

    //  top->io_fromTSHRCtrl_RXSNP_ready = 0;
        top->io_fromTSHRCtrl_RXSNP_valid = 0;
        top->io_fromTSHRCtrl_RXSNP_bits_QoS = 0;
        top->io_fromTSHRCtrl_RXSNP_bits_SrcID = 0;
        top->io_fromTSHRCtrl_RXSNP_bits_TxnID = 0;
        top->io_fromTSHRCtrl_RXSNP_bits_FwdNID = 0;
        top->io_fromTSHRCtrl_RXSNP_bits_FwdTxnID_StashLPIDValid_StashLPID_VMIDExt = 0;
        top->io_fromTSHRCtrl_RXSNP_bits_Opcode = 0;
        top->io_fromTSHRCtrl_RXSNP_bits_Addr = 0;
        top->io_fromTSHRCtrl_RXSNP_bits_NS = 0;
        top->io_fromTSHRCtrl_RXSNP_bits_DoNotGoToSD_DoNotDataPull = 0;
        top->io_fromTSHRCtrl_RXSNP_bits_RetToSrc = 0;
        top->io_fromTSHRCtrl_RXSNP_bits_TraceTag = 0;

    //  top->io_fromTSHRCtrl_RXREQ_ready = 0;
        top->io_fromTSHRCtrl_RXREQ_valid = 0;
        top->io_fromTSHRCtrl_RXREQ_bits_TxnID = 0;
        top->io_fromTSHRCtrl_RXREQ_bits_Opcode = 0;
        top->io_fromTSHRCtrl_RXREQ_bits_Size = 0;
        top->io_fromTSHRCtrl_RXREQ_bits_NS = 0;
        top->io_fromTSHRCtrl_RXREQ_bits_Order = 0;
        top->io_fromTSHRCtrl_RXREQ_bits_MemAttr = 0;
        top->io_fromTSHRCtrl_RXREQ_bits_Excl = 0;
        top->io_fromTSHRCtrl_RXREQ_bits_WayValid = 0;
        top->io_fromTSHRCtrl_RXREQ_bits_Way = 0;
        top->io_fromTSHRCtrl_RXREQ_bits_TraceTag = 0;
        top->io_fromTSHRCtrl_RXREQ_bits_Addr = 0;
    }
    
    inline BundleFromTSHRCtrl::~BundleFromTSHRCtrl() noexcept 
    { }
}

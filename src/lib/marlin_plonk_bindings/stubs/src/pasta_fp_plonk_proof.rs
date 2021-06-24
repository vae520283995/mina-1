use crate::arkworks::{CamlDlogProofVesta, CamlFp, CamlGroupAffineVesta, CamlPolyCommVesta};
use crate::pasta_fp_plonk_index::CamlPastaFpPlonkIndexPtr;
use crate::pasta_fp_plonk_verifier_index::CamlPastaFpPlonkVerifierIndex;
use crate::pasta_fp_vector::CamlPastaFpVector;
use ark_ec::AffineCurve;
use ark_ff::One;
use commitment_dlog::commitment::{CommitmentCurve, OpeningProof, PolyComm};
use groupmap::GroupMap;
use mina_curves::pasta::{
    fp::Fp,
    fq::Fq,
    vesta::{Affine as GAffine, VestaParameters},
};
use oracle::{
    poseidon::PlonkSpongeConstants,
    sponge::{DefaultFqSponge, DefaultFrSponge},
};
use plonk_circuits::scalars::ProofEvaluations as DlogProofEvaluations;
use plonk_protocol_dlog::index::{Index as DlogIndex, VerifierIndex as DlogVerifierIndex};
use plonk_protocol_dlog::prover::{ProverCommitments as DlogCommitments, ProverProof as DlogProof};

#[ocaml::func]
pub fn caml_pasta_fp_plonk_proof_create(
    index: CamlPastaFpPlonkIndexPtr<'static>,
    primary_input: CamlPastaFpVector,
    auxiliary_input: CamlPastaFpVector,
    prev_challenges: Vec<CamlFp>,
    prev_sgs: Vec<CamlGroupAffineVesta>,
) -> CamlDlogProofVesta {
    // TODO: Should we be ignoring this?!
    let _primary_input = primary_input;

    let prev: Vec<(Vec<Fp>, PolyComm<GAffine>)> = {
        if prev_challenges.len() == 0 {
            Vec::new()
        } else {
            let challenges_per_sg = prev_challenges.len() / prev_sgs.len();
            prev_sgs
                .into_iter()
                .map(Into::<GAffine>::into)
                .enumerate()
                .map(|(i, sg)| {
                    (
                        prev_challenges[(i * challenges_per_sg)..(i + 1) * challenges_per_sg]
                            .iter()
                            .map(Into::<Fp>::into)
                            .collect(),
                        PolyComm::<GAffine> {
                            unshifted: vec![sg.into()],
                            shifted: None,
                        },
                    )
                })
                .collect()
        }
    };

    let auxiliary_input: &Vec<Fp> = &*auxiliary_input;
    let index: &DlogIndex<GAffine> = &index.as_ref().0;

    ocaml::runtime::release_lock();

    let map = GroupMap::<Fq>::setup();
    let proof = DlogProof::create::<
        DefaultFqSponge<VestaParameters, PlonkSpongeConstants>,
        DefaultFrSponge<Fp, PlonkSpongeConstants>,
    >(&map, auxiliary_input, index, prev)
    .unwrap();

    ocaml::runtime::acquire_lock();

    proof.into()
}

pub fn proof_verify(
    lgr_comm: Vec<PolyComm<GAffine>>,
    index: &DlogVerifierIndex<GAffine>,
    proof: DlogProof<GAffine>,
) -> bool {
    let group_map = <GAffine as CommitmentCurve>::Map::setup();

    DlogProof::verify::<
        DefaultFqSponge<VestaParameters, PlonkSpongeConstants>,
        DefaultFrSponge<Fp, PlonkSpongeConstants>,
    >(
        &group_map,
        &[(
            index,
            &lgr_comm.into_iter().map(From::from).collect(),
            &proof,
        )]
        .to_vec(),
    )
    .is_ok()
}

#[ocaml::func]
pub fn caml_pasta_fp_plonk_proof_verify(
    lgr_comm: Vec<CamlPolyCommVesta>,
    index: CamlPastaFpPlonkVerifierIndex,
    proof: CamlDlogProofVesta,
) -> bool {
    let lgr_comm = lgr_comm.iter().map(|x| x.into()).collect();
    proof_verify(lgr_comm, &index.into(), proof.into())
}

#[ocaml::func]
pub fn caml_pasta_fp_plonk_proof_batch_verify(
    lgr_comms: Vec<Vec<CamlPolyCommVesta>>,
    indexes: Vec<CamlPastaFpPlonkVerifierIndex>,
    proofs: Vec<CamlDlogProofVesta>,
) -> bool {
    let ts: Vec<_> = indexes
        .into_iter()
        .zip(lgr_comms.into_iter())
        .zip(proofs.into_iter())
        .map(|((i, l), p)| (i.into(), l.into_iter().map(Into::into).collect(), p.into()))
        .collect();
    let ts: Vec<_> = ts.iter().map(|(i, l, p)| (i, l, p)).collect();
    let group_map = GroupMap::<Fq>::setup();

    DlogProof::<GAffine>::verify::<
        DefaultFqSponge<VestaParameters, PlonkSpongeConstants>,
        DefaultFrSponge<Fp, PlonkSpongeConstants>,
    >(&group_map, &ts)
    .is_ok()
}

#[ocaml::func]
pub fn caml_pasta_fp_plonk_proof_dummy() -> CamlDlogProofVesta {
    let g = || GAffine::prime_subgroup_generator();
    let comm = || PolyComm {
        shifted: Some(g()),
        unshifted: vec![g(), g(), g()],
    };
    let dlogproof = DlogProof {
        prev_challenges: vec![
            (vec![Fp::one(), Fp::one()], comm()),
            (vec![Fp::one(), Fp::one()], comm()),
            (vec![Fp::one(), Fp::one()], comm()),
        ],
        proof: OpeningProof {
            lr: vec![(g(), g()), (g(), g()), (g(), g())],
            z1: Fp::one(),
            z2: Fp::one(),
            delta: g(),
            sg: g(),
        },
        commitments: DlogCommitments {
            l_comm: comm(),
            r_comm: comm(),
            o_comm: comm(),
            z_comm: comm(),
            t_comm: comm(),
        },
        public: vec![Fp::one(), Fp::one()],
        evals: {
            let evals = || vec![Fp::one(), Fp::one(), Fp::one(), Fp::one()];
            let evals = || DlogProofEvaluations {
                l: evals(),
                r: evals(),
                o: evals(),
                z: evals(),
                t: evals(),
                f: evals(),
                sigma1: evals(),
                sigma2: evals(),
            };
            [evals(), evals()]
        },
    };
    dlogproof.into()
}

#[ocaml::func]
pub fn caml_pasta_fp_plonk_proof_deep_copy(x: CamlDlogProofVesta) -> CamlDlogProofVesta {
    x
}

#!/usr/bin/env -S deno run --allow-net --allow-read --allow-run --allow-write

async function fetchJSON(url) {
  const r = await fetch(url);
  if (!r.ok) throw new Error(await r.text());
  return r.json();
}

async function fetchGitLabArtifactURL(
  gitlab_domain,
  project_id,
  ref,
  job_name,
  artifact_path,
  pipeline_filter
) {
  const pipeline_id = (
    await fetchJSON(
      `https://${gitlab_domain}/api/v4/projects/${project_id}/pipelines?ref=${ref}${pipeline_filter}`
    )
  )[0].id;
  const job_id = (
    await fetchJSON(
      `https://${gitlab_domain}/api/v4/projects/${project_id}/pipelines/${pipeline_id}/jobs?per_page=100`
    )
  ).find((e) => e.name === job_name).id;
  return artifact_path
    ? `https://${gitlab_domain}/api/v4/projects/${project_id}/jobs/${job_id}/artifacts/${artifact_path}`
    : `https://${gitlab_domain}/api/v4/projects/${project_id}/jobs/${job_id}/artifacts`;
}

async function fetchGitHubArtifactURL(
  owner,
  repo,
  branch,
  workflow_name,
  artifact_name,
  event
) {
  const run_id = (
    await fetchJSON(
      `https://api.github.com/repos/${owner}/${repo}/actions/runs?branch=${branch}&event=${event}`
    )
  ).workflow_runs.find((e) => e.name && e.name === workflow_name).id;
  const artifact_id = (
    await fetchJSON(
      `https://api.github.com/repos/${owner}/${repo}/actions/runs/${run_id}/artifacts`
    )
  ).artifacts.find((e) => e.name === artifact_name).id;
  return `https://nightly.link/${owner}/${repo}/actions/artifacts/${artifact_id}.zip`;
}

async function fetchGitHubLatestReleaseURL(owner, repo, suffix) {
  return (
    await fetchJSON(
      `https://api.github.com/repos/${owner}/${repo}/releases/latest`
    )
  ).assets.find((e) => e.name.endsWith(suffix)).browser_download_url;
}

function parseActualHash(msg) {
  return Array.from(msg.matchAll(/sha256:[0-9a-z]+/g))
    .at(-1)[0]
    .split("sha256:")[1];
}

async function fetchHash(fetcher, fetcher_opts) {
  const proc = Deno.run({
    cmd: [
      "nix",
      "eval",
      "--expr",
      `${fetcher}(builtins.fromJSON(${JSON.stringify(
        JSON.stringify(fetcher_opts)
      )}))`,
    ],
    stdin: "null",
    stdout: "null",
    stderr: "piped",
  });
  const msg = new TextDecoder().decode(await proc.stderrOutput());
  const hash = parseActualHash(msg);
  await proc.status();
  return hash;
}

async function fetchGitLabArtifact(
  fetcher,
  gitlab_domain,
  project_id,
  ref,
  job_name,
  artifact_path,
  pipeline_filter = ""
) {
  const url = await fetchGitLabArtifactURL(
    gitlab_domain,
    project_id,
    ref,
    job_name,
    artifact_path,
    pipeline_filter
  );
  const sha256 = await fetchHash(fetcher, { url, sha256: "" });
  return { url, sha256 };
}

async function fetchGitHubArtifact(
  owner,
  repo,
  branch,
  workflow_name,
  artifact_name,
  event
) {
  const url = await fetchGitHubArtifactURL(
    owner,
    repo,
    branch,
    workflow_name,
    artifact_name,
    event
  );
  const sha256 = await fetchHash("builtins.fetchTarball", { url, sha256: "" });
  return { url, sha256 };
}

async function fetchGitHubLatestRelease(fetcher, owner, repo, suffix) {
  const url = await fetchGitHubLatestReleaseURL(owner, repo, suffix);
  const sha256 = await fetchHash(fetcher, { url, sha256: "" });
  return { url, sha256 };
}

async function fetchTarball(url) {
  const sha256 = await fetchHash("builtins.fetchTarball", { url, sha256: "" });
  return { url, sha256 };
}

async function fetchurl(url) {
  const sha256 = await fetchHash("builtins.fetchurl", { url, sha256: "" });
  return { url, sha256 };
}

const _wasm32_wasi_ghc_gmp = fetchGitLabArtifact(
  "builtins.fetchTarball",
  "gitlab.haskell.org",
  1,
  "master",
  "nightly-x86_64-linux-alpine3_17-wasm-cross_wasm32-wasi-release+fully_static",
  "ghc-x86_64-linux-alpine3_17-wasm-cross_wasm32-wasi-release+fully_static.tar.xz",
  "&source=schedule"
);
const _wasm32_wasi_ghc_native = fetchGitLabArtifact(
  "builtins.fetchTarball",
  "gitlab.haskell.org",
  1,
  "master",
  "nightly-x86_64-linux-alpine3_17-wasm-int_native-cross_wasm32-wasi-release+fully_static",
  "ghc-x86_64-linux-alpine3_17-wasm-int_native-cross_wasm32-wasi-release+fully_static.tar.xz",
  "&source=schedule"
);
const _wasm32_wasi_ghc_unreg = fetchGitLabArtifact(
  "builtins.fetchTarball",
  "gitlab.haskell.org",
  1,
  "master",
  "nightly-x86_64-linux-alpine3_17-wasm-unreg-cross_wasm32-wasi-release+fully_static",
  "ghc-x86_64-linux-alpine3_17-wasm-unreg-cross_wasm32-wasi-release+fully_static.tar.xz",
  "&source=schedule"
);
const _wasm32_wasi_ghc_9_6 = fetchGitLabArtifact(
  "builtins.fetchTarball",
  "gitlab.haskell.org",
  3223,
  "ghc-9.6",
  "x86_64-linux-alpine3_12-cross_wasm32-wasi-release+fully_static",
  "ghc-x86_64-linux-alpine3_12-cross_wasm32-wasi-release+fully_static.tar.xz"
);
const _wasi_sdk = fetchGitLabArtifact(
  "builtins.fetchTarball",
  "gitlab.haskell.org",
  3212,
  "main",
  "x86_64-linux",
  "dist/wasi-sdk-17-linux.tar.gz",
  "&status=success"
);
const _libffi_wasm = fetchGitLabArtifact(
  "builtins.fetchTarball",
  "gitlab.haskell.org",
  3214,
  "master",
  "x86_64-linux",
  null
);
const _deno = fetchGitHubLatestRelease(
  "builtins.fetchTarball",
  "denoland",
  "deno",
  "unknown-linux-gnu.zip"
);
const _nodejs = fetchTarball(
  "https://unofficial-builds.nodejs.org/download/release/v20.0.0/node-v20.0.0-linux-x64-pointer-compression.tar.xz"
);
const _bun = fetchGitHubLatestRelease(
  "builtins.fetchTarball",
  "oven-sh",
  "bun",
  "linux-x64.zip"
);
const _binaryen = fetchGitHubArtifact(
  "type-dance",
  "binaryen",
  "main",
  "release",
  "release-ubuntu-latest",
  "push"
);
const _wabt = fetchGitHubLatestRelease(
  "builtins.fetchTarball",
  "WebAssembly",
  "wabt",
  "ubuntu.tar.gz"
);
const _wasmtime = fetchGitHubLatestRelease(
  "builtins.fetchTarball",
  "bytecodealliance",
  "wasmtime",
  "x86_64-linux.tar.xz"
);
const _wasmedge = fetchGitHubLatestRelease(
  "builtins.fetchTarball",
  "WasmEdge",
  "WasmEdge",
  "ubuntu20.04_x86_64.tar.gz"
);
const _wazero = fetchGitHubLatestRelease(
  "builtins.fetchTarball",
  "tetratelabs",
  "wazero",
  "linux_amd64.tar.gz"
);
const _wizer = fetchGitHubArtifact(
  "bytecodealliance",
  "wizer",
  "main",
  "Release",
  "bins-x86_64-linux",
  "push"
);
const _cabal = fetchurl(
  "https://downloads.haskell.org/cabal/cabal-install-3.10.1.0/cabal-install-3.10.1.0-x86_64-linux-alpine.tar.xz"
);
const _proot = fetchGitLabArtifact(
  "builtins.fetchurl",
  "gitlab.com",
  9799675,
  "master",
  "dist",
  "dist/proot"
);

await Deno.writeTextFile(
  "autogen.json",
  JSON.stringify(
    {
      "wasm32-wasi-ghc-gmp": await _wasm32_wasi_ghc_gmp,
      "wasm32-wasi-ghc-native": await _wasm32_wasi_ghc_native,
      "wasm32-wasi-ghc-unreg": await _wasm32_wasi_ghc_unreg,
      "wasm32-wasi-ghc-9.6": await _wasm32_wasi_ghc_9_6,
      "wasi-sdk": await _wasi_sdk,
      "libffi-wasm": await _libffi_wasm,
      deno: await _deno,
      nodejs: await _nodejs,
      bun: await _bun,
      binaryen: await _binaryen,
      wabt: await _wabt,
      wasmtime: await _wasmtime,
      wasmedge: await _wasmedge,
      wazero: await _wazero,
      wizer: await _wizer,
      cabal: await _cabal,
      proot: await _proot,
    },
    null,
    2
  )
);

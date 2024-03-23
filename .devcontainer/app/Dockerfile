# FROM --platform=linux/amd64 ubuntu:22.04
# FROM --platform=linux/amd64 nvidia/cuda:11.8.0-cudnn8-runtime-ubuntu22.04
FROM --platform=linux/amd64 nvidia/cuda:11.8.0-base-ubuntu22.04

# # install build essentials
RUN apt-get update \
  && export DEBIAN_FRONTEND=noninteractive \
  && apt-get install -y build-essential git-core ntpdate curl wget nano procps

RUN apt-get install -y python3 python3-pip
# All we need is the Nvidia driver as PyTorch comes with
# cudnn, cuda-runtime, nvrtc, cublas, cusparse, nccl, cufft, triton, etc.
# https://discuss.pytorch.org/t/how-to-check-if-torch-uses-cudnn/21933/3
# install pytorch
RUN pip3 install torch torchvision torchaudio --index-url https://download.pytorch.org/whl/cu118
# install legacy package ptan to make things work with the DRL book
# for more information on using Tensorboard with PyTorch, see link
# https://pytorch.org/tutorials/recipes/recipes/tensorboard_with_pytorch.html#run-tensorboard
RUN pip3 install ptan "gym[atari]==0.25.2" opencv-python-headless tensorboard matplotlib pandas

# TODO: substitute by gymnasium and opencv
# RUN pip3 install "gymnasium[atari]" opencv-python


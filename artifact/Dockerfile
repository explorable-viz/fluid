FROM node:20-slim

WORKDIR /usr/src/app


RUN apt-get update
RUN apt-get install -y wget gnupg
RUN wget -q -O - https://dl-ssl.google.com/linux/linux_signing_key.pub | apt-key add -
RUN echo "deb http://dl.google.com/linux/chrome/deb/ stable main" > /etc/apt/sources.list.d/google.list
RUN apt-get update
RUN apt-get install -y google-chrome-stable xvfb

RUN apt-get update 
RUN apt-get install -y ca-certificates git zip unzip python3 python3-venv python3-pip texlive-latex-base texlive-latex-extra
RUN python3 -m venv /usr/src/python/
RUN /usr/src/python/bin/pip3 install numpy pandas matplotlib jinja2
RUN apt-get update && apt-get -y install sudo
RUN sudo apt-get install libtinfo5
RUN sudo apt-get clean && rm -rf /var/lib/apt/lists/*

COPY package.json ./
COPY yarn.lock ./

RUN yarn install

COPY . ./

EXPOSE 8080

RUN useradd -ms /bin/bash evaluator && \
    echo "evaluator ALL=(ALL) NOPASSWD:ALL" >> /etc/sudoers

RUN chown -R evaluator /usr/src/app/benchmark

USER evaluator

RUN sudo yarn build
RUN sudo yarn bundle-website -w Esop2025Artifact
RUN sudo yarn bundle-website -w Misc

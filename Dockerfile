FROM node

WORKDIR /usr/src/app
COPY . /usr/src/app

RUN apt-get update && apt-get -y install sudo
RUN sudo apt-get install libtinfo5
RUN curl --compressed -o- -L https://yarnpkg.com/install.sh | bash
RUN yarn install && yarn run bundle-app

CMD yarn parcel serve index.html

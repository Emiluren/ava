#ifndef SPRITER_HELPERS_HPP
#define SPRITER_HELPERS_HPP

#include <spriterengine/override/filefactory.h>
#include <spriterengine/override/imagefile.h>
#include "jsonspriterfiledocumentwrapper.h"
#include <iostream>
#include <functional>

using namespace std;
using namespace SpriterEngine;

class HaskellSprite {};

struct SpriteState {
    double alpha;
    point position;
    double angle;
    point scale;
    point pivot;
};

void print_sprite_state(SpriteState* state) {
    printf("SpriteState { alpha = %f, position = (%f, %f), angle = %f, scale = (%f, %f), pivot = (%f, %f) }\n",
           state->alpha, state->position.x, state->position.y, state->angle,
           state->scale.x, state->scale.y, state->pivot.x, state->pivot.y);
}

class SpriterImageFile : public ImageFile
{
public:
    SpriterImageFile(HaskellSprite* sprite,
                     function<void(HaskellSprite*, SpriteState*)> render,
                     const string& filename,
                     point pivot) :
        sprite(sprite),
        ImageFile(filename, pivot),
        render(render) {
    }

    void renderSprite(UniversalObjectInterface *spriteInfo) override {
        SpriteState state;
        state.alpha = spriteInfo->getAlpha();
        state.position = spriteInfo->getPosition();
        state.angle = spriteInfo->getAngle();
        state.scale = spriteInfo->getScale();
        state.pivot = spriteInfo->getPivot();

        //print_sprite_state(&state);
        render(sprite, &state);
    }

private:
    HaskellSprite* sprite;
    function<void(HaskellSprite*, SpriteState*)> render = nullptr;
};

class SpriterFileFactory : public FileFactory
{
public:
    SpriterFileFactory(function<HaskellSprite*(const char*, double, double)> imageLoad,
                       function<void(HaskellSprite*, SpriteState*)> render) :
        imageLoad(imageLoad), render(render) {
    }

    ImageFile *newImageFile(const string& filename,
                            point pivot,
                            atlasdata atlasData) override {
        HaskellSprite* sprite = imageLoad(filename.c_str(), pivot.x, pivot.y);
        return new SpriterImageFile(sprite, render, filename, pivot);
    }

	SpriterFileDocumentWrapper* newSconDocumentWrapper() override {
        return new JSONSpriterFileDocumentWrapper();
    }

private:
    function<HaskellSprite*(const char*, double, double)> imageLoad;
    function<void(HaskellSprite*, SpriteState*)> render;
};

#endif

#pragma once
// David Miller, http://millermattson.com/dave
// See the associated video for instructions: http://vimeo.com/19569529

#include <iostream>
#include <cstdlib>
#include <cassert>
#include <cmath>
#include <fstream>
#include <sstream>
#include <vector>

#if 0
using REAL = double;
#else
using REAL = float;
#endif

struct Connection
{
    REAL weight = 0;
    REAL deltaWeight = 0;
};

class Neuron;

using Layer = std::vector<Neuron>;

class Neuron {
public:
    explicit Neuron(unsigned numOutputs, unsigned myIndex);
    void setOutputVal(REAL val) { m_outputVal = val; }
    REAL getOutputVal(void) const { return m_outputVal; }
    void feedForward(const Layer &prevLayer);
    void calcOutputGradients(REAL targetVal);
    void calcHiddenGradients(const Layer &nextLayer);
    void updateInputWeights(Layer &prevLayer);

    void copyWeightsFrom(const Neuron& other) {
        m_outputWeights = other.m_outputWeights;
    }

    void saveToFile(FILE *fp) const {
        for (auto&& connection : m_outputWeights) {
            fprintf(fp, " %.3f", double(connection.weight));
        }
        fprintf(fp, "\n");
    }

    void loadFromFile(FILE *fp) {
        for (auto&& connection : m_outputWeights) {
            double temp;
            fscanf(fp, "%lf", &temp);
            connection.weight = temp;
        }
    }

private:
    static constexpr REAL eta = 0.015;   // [0.0..1.0] overall net training rate
    static constexpr REAL alpha = 0.05; // [0.0..n] multiplier of last weight change (momentum)
    static REAL transferFunction(REAL x);
    static REAL transferFunctionDerivative(REAL x);
    static REAL randomWeight(void) { return rand() / REAL(RAND_MAX) - 0.5; }
    REAL sumDOW(const Layer &nextLayer) const;
    REAL m_outputVal;
    std::vector<Connection> m_outputWeights;
    unsigned m_myIndex;
    REAL m_gradient;
};

inline void Neuron::updateInputWeights(Layer &prevLayer)
{
    // The weights to be updated are in the Connection container
    // in the neurons in the preceding layer

    for (Neuron& neuron : prevLayer) {
        REAL oldDeltaWeight = neuron.m_outputWeights[m_myIndex].deltaWeight;

        REAL newDeltaWeight =
                // Individual input, magnified by the gradient and train rate:
                eta
                * neuron.getOutputVal()
                * m_gradient
                // Also add momentum = a fraction of the previous delta weight;
                + alpha
                * oldDeltaWeight;

        neuron.m_outputWeights[m_myIndex].deltaWeight = newDeltaWeight;
        neuron.m_outputWeights[m_myIndex].weight += newDeltaWeight;
    }
}

inline REAL Neuron::sumDOW(const Layer &nextLayer) const
{
    REAL sum = 0.0;

    // Sum our contributions of the errors at the nodes we feed.

    for (unsigned n = 0; n < nextLayer.size() - 1; ++n) {
        sum += m_outputWeights[n].weight * nextLayer[n].m_gradient;
    }

    return sum;
}

inline void Neuron::calcHiddenGradients(const Layer &nextLayer)
{
    REAL dow = sumDOW(nextLayer);
    m_gradient = dow * Neuron::transferFunctionDerivative(m_outputVal);
}

inline void Neuron::calcOutputGradients(REAL targetVal)
{
    REAL delta = targetVal - m_outputVal;
    m_gradient = delta * Neuron::transferFunctionDerivative(m_outputVal);
}

inline REAL Neuron::transferFunction(REAL x)
{
#if 0
    return tanh(x);  // -1 to +1
#else
    return 1.0 / (1.0 + exp(-x));  // 0 to +1
#endif
}

inline REAL Neuron::transferFunctionDerivative(REAL x)
{
#if 0
    // tanh derivative
    return 1.0 - x * x;
#else
    REAL sx = 1.0 / (1.0 + exp(-x));
    return sx * (1.0 - sx);
#endif
}

inline void Neuron::feedForward(const Layer &prevLayer)
{
    REAL sum = 0.0;

    // Sum the previous layer's outputs (which are our inputs)
    // Include the bias node from the previous layer.

    for (const Neuron& neuron : prevLayer) {
        sum += neuron.getOutputVal() * neuron.m_outputWeights[m_myIndex].weight;
    }

    m_outputVal = Neuron::transferFunction(sum);
}

inline Neuron::Neuron(unsigned numOutputs, unsigned myIndex) : m_outputWeights(numOutputs), m_myIndex(myIndex)
{
    for (auto&& connection : m_outputWeights) {
        connection.weight = randomWeight();
    }
}


// ****************** class Net ******************
class Net
{
public:
    explicit Net(const std::vector<unsigned> &topology);

    template<class... Inputs>
    void feedForward(const Inputs&... inputVals);

    void backProp(const std::vector<REAL> &targetVals);
    std::vector<REAL> getResults() const;

    void copyWeightsFrom(int a, const Net& other, int b) {
        assert(m_layers[a].size() == other.m_layers[b].size());
        for (size_t i = 0; i < m_layers[a].size(); ++i) {
            m_layers[a][i].copyWeightsFrom(other.m_layers[b][i]);
        }
    }

    void saveLayerToFile(int a, FILE *fp) const {
        const Layer &layer = m_layers[a];
        for (const Neuron& neuron : layer) {
            neuron.saveToFile(fp);
        }
    }

    void loadLayerFromFile(int a, FILE *fp) {
        Layer &layer = m_layers[a];
        for (Neuron& neuron : layer) {
            neuron.loadFromFile(fp);
        }
    }

private:
    std::vector<Layer> m_layers; // m_layers[layerNum][neuronNum]
    REAL m_error;
};

inline std::vector<REAL> Net::getResults() const
{
    const Layer& layer = m_layers.back();
    size_t result_size = layer.size() - 1;
    std::vector<REAL> resultVals(result_size);
    for (size_t n = 0; n < result_size; ++n) {
        resultVals[n] = layer[n].getOutputVal();
    }
    return resultVals;
}

inline void Net::backProp(const std::vector<REAL> &targetVals)
{
    // Calculate overall net error (RMS of output neuron errors)

    Layer &outputLayer = m_layers.back();
    m_error = 0.0;

    for (unsigned n = 0; n < outputLayer.size() - 1; ++n) {
        REAL delta = targetVals[n] - outputLayer[n].getOutputVal();
        m_error += delta * delta;
    }
    m_error /= outputLayer.size() - 1; // get average error squared
    m_error = sqrt(m_error); // RMS

    // Calculate output layer gradients

    for (unsigned n = 0; n < outputLayer.size() - 1; ++n) {
        outputLayer[n].calcOutputGradients(targetVals[n]);
    }

    // Calculate hidden layer gradients

    for (unsigned layerNum = m_layers.size() - 2; layerNum > 0; --layerNum) {
        Layer &hiddenLayer = m_layers[layerNum];
        Layer &nextLayer = m_layers[layerNum + 1];

        for (unsigned n = 0; n < hiddenLayer.size(); ++n) {
            hiddenLayer[n].calcHiddenGradients(nextLayer);
        }
    }

    // For all layers from outputs to first hidden layer,
    // update connection weights

    for (unsigned layerNum = m_layers.size() - 1; layerNum > 0; --layerNum) {
        Layer &layer = m_layers[layerNum];
        Layer &prevLayer = m_layers[layerNum - 1];

        for (unsigned n = 0; n < layer.size() - 1; ++n) {
            layer[n].updateInputWeights(prevLayer);
        }
    }
}

template<class... Inputs>
inline void Net::feedForward(const Inputs&... inputVals)
{
    // Assign (latch) the input values into the input neurons
    size_t k = 0;
    int dummy[] = {
        [&](const auto& in) {
            for (auto&& val : in) {
                m_layers[0][k].setOutputVal(val);
                ++k;
            }
            return 0;
        }(inputVals)...
    };
    (void)dummy;
    assert(k == m_layers[0].size() - 1);

    // forward propagate
    for (unsigned layerNum = 1; layerNum < m_layers.size(); ++layerNum) {
        Layer &prevLayer = m_layers[layerNum - 1];
        for (unsigned n = 0; n < m_layers[layerNum].size() - 1; ++n) {
            m_layers[layerNum][n].feedForward(prevLayer);
        }
    }
}

inline Net::Net(const std::vector<unsigned> &topology)
{
    unsigned numLayers = topology.size();
    m_layers.reserve(numLayers);
    for (unsigned layerNum = 0; layerNum < numLayers; ++layerNum) {
        m_layers.push_back(Layer());
        unsigned numOutputs = layerNum == topology.size() - 1 ? 0 : topology[layerNum + 1];

        // We have a new layer, now fill it with neurons, and
        // add a bias neuron in each layer.
        m_layers.back().reserve(topology[layerNum] + 1);
        for (unsigned neuronNum = 0; neuronNum <= topology[layerNum]; ++neuronNum) {
            m_layers.back().push_back(Neuron(numOutputs, neuronNum));
        }

        // Force the bias node's output to 1.0 (it was the last neuron pushed in this layer):
        m_layers.back().back().setOutputVal(1.0);
    }
}


inline std::string vec_to_string(const std::vector<REAL>& vec)
{
    std::string result;
    for (REAL f : vec) {
        if (-0.5 < f && f < 0.5) result += '0';
        else if (f < 0) result += '-';
        else if (f < 1.5) result += '1';
        else result += '+';
    }
    return result;
}

inline std::vector<REAL> vec_concat(const std::vector<REAL>& a, const std::vector<REAL>& b)
{
    std::vector<REAL> result;
    result.reserve(a.size() + b.size());
    result.insert(result.end(), a.begin(), a.end());
    result.insert(result.end(), b.begin(), b.end());
    return result;
}
